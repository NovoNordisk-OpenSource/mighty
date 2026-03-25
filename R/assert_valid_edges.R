#' Validate Dependency Graph Edges
#'
#' Validates that all nodes connect back to their domain's init_domain node.
#'
#' @param edges Data.table with parent_node and node_id columns
#' @param nodes Data.table with node_id, domain, and type columns
#'
#' @return Invisibly returns edges if valid, aborts on errors
#' @noRd
assert_valid_edges <- function(edges, nodes) {
  checkmate::assert_data_table(edges, null.ok = FALSE)
  checkmate::assert_data_table(nodes, null.ok = FALSE)

  # Check for required columns in edges
  checkmate::assert_names(
    names(edges),
    must.include = c("parent_node", "node_id"),
    .var.name = "edges column names"
  )

  # Check for required columns in nodes
  checkmate::assert_names(
    names(nodes),
    must.include = c("node_id", "domain", "type"),
    .var.name = "nodes column names"
  )

  validate_init_domain_presence(nodes)

  # Get ALL init_domain nodes across all domains
  init_domain_nodes <- nodes[nodes$type == "init_domain", ]$node_id

  # Get all nodes that need validation (exclude col_copy and init_domain)
  all_nodes <- nodes[!nodes$type %in% c("col_copy", "init_domain")]

  # Single traversal from all init_domain nodes
  unconnected_nodes <- find_unconnected_nodes(
    all_nodes,
    edges,
    init_domain_nodes
  )

  if (nrow(unconnected_nodes) > 0) {
    report_issues(unconnected_nodes)
  }

  return(invisible(edges))
}

#' Validate Presence of init_domain Nodes
#'
#' Checks that each domain has at least one init_domain node. Aborts with an
#' error if any domain is missing its init_domain node. This is not an error
#' we expect users to encounter, but we are keeping it as a defensive validator
#' to ensure any future refactoring does not introduce this issues in the
#' topology.
#'
#' @param nodes Data.table with node_id, domain, and type columns
#'
#' @return Invisible NULL if valid, aborts on errors
#' @noRd
validate_init_domain_presence <- function(nodes) {
  init_counts <- nodes[, .(has_init = sum(type == "init_domain")), by = domain]

  missing_domain <- init_counts[has_init == 0]$domain[1]
  no_missing_domains <- is.na(missing_domain)

  if (no_missing_domains) {
    return(invisible(NULL))
  }

  domain_msg <- paste0(
    "The init_domain node is missing for ",
    format_domain(missing_domain)
  )

  throw_validation_error(
    category = "Topology errors",
    details = c(
      "i" = domain_msg,
      "i" = "Something is wrong in your specification. Please contact the Mighty developers"
    ),
    suggestions = c(
      "This is likely a bug in the specification processing",
      "Contact the Mighty development team with your specification for investigation"
    )
  )
}


#' Find Nodes Not Connected to init_domain Parents
#'
#' @param all_nodes Data.table of nodes to check
#' @param edges Data.table with parent_node and node_id columns
#' @param init_domain_nodes Character vector of init_domain nodes to start from
#'
#' @return Data.table with nodes that cannot be reached from any init_domain
#' @noRd
find_unconnected_nodes <- function(all_nodes, edges, init_domain_nodes) {
  # Use single "discovered" set and index-based queue
  discovered <- init_domain_nodes
  queue <- init_domain_nodes
  head_idx <- 1

  while (head_idx <= length(queue)) {
    current <- queue[head_idx]
    head_idx <- head_idx + 1

    children <- edges[edges$parent_node == current, ]$node_id
    new_children <- children[!children %in% discovered]

    if (length(new_children) > 0) {
      discovered <- c(discovered, new_children)
      queue <- c(queue, new_children)
    }
  }

  # Return nodes not reachable from any init_domain
  all_nodes[!all_nodes$node_id %in% discovered, ]
}


#' Report Dependency Validation Issues
#'
#' @param issues Data.table of nodes with validation failures
#'
#' @return Invisibly returns NULL if no issues; otherwise throws an error
#' @noRd
report_issues <- function(issues) {
  no_issues <- nrow(issues) == 0
  if (no_issues) {
    return(invisible(NULL))
  }
  node_analyses <- lapply(seq_len(nrow(issues)), function(i) {
    analyze_node_validation_issue(issues[i, ])
  })

  # List unconnected nodes
  analyses_text <- setNames(
    unlist(node_analyses),
    rep("i", length(node_analyses))
  )

  n <- nrow(issues)

  count_msg <- cli::format_inline(
    "ADaM specification{?/s} contain{?/s} {n} inconsistent declaration{?/s} of dependencies"
  )

  # Analyze what types of nodes failed to provide contextual suggestions
  has_col_compute <- any(issues$type == "col_compute")
  has_unexpected_types <- any(!issues$type %in% c("col_compute"))

  # Build conditional suggestions based on node types
  suggestions <- c(
    "Ensure all dependencies reference columns from the same domain or parent domains",
    "Check that cross-domain dependencies are properly defined",
    "Verify all required domains are included in your specifications",
    "Review dependency chains to ensure they connect to domain initialization"
  )

  if (has_col_compute) {
    suggestions <- c(
      "Check for missing @depends annotations in components that create derived columns",
      suggestions
    )
  }

  if (has_unexpected_types) {
    suggestions <- c(
      "This likely indicates an internal bug in Mighty - please report to the developers with your specification",
      suggestions
    )
  }

  throw_validation_error(
    category = "Unconnected nodes",
    details = c(
      "i" = count_msg,
      "i" = "Mighty cannot create the necessary dependency relationships",
      "",
      "Affected nodes:",
      analyses_text
    ),
    suggestions = suggestions
  )
}

#' Analyze Individual Node Validation Issue
#'
#' @param node A single row data.table representing one node
#' @return Character string with analysis of this specific node
#' @noRd
analyze_node_validation_issue <- function(node) {
  # Extract and format node information
  outputs <- extract_node_outputs(node)
  outputs_highlighted <- format_outputs_for_display(outputs)
  depend_cols <- extract_node_dependencies(node)
  depend_cols_highlighted <- format_dependencies_for_display(depend_cols)

  component_info <- if (!is.na(node$code_id) && !is.null(node$code_id)) {
    component_name <- basename(node$code_id)
    paste0("via {.file ", component_name, "}")
  } else {
    ""
  }

  # Generate specific analysis based on node type
  issue_analysis <- generate_node_type_analysis(node, outputs)

  # Build the node analysis message with color highlighting
  analysis_parts <- c(
    paste0("Node ", node$node_id),
    paste0("domain: ", format_domain(node$domain)),
    paste0("outputs: ", outputs_highlighted, component_info),
    paste0("dependencies: ", depend_cols_highlighted),
    paste0("probable cause: ", issue_analysis)
  )

  paste(analysis_parts, collapse = ", ")
}

#' @noRd
extract_node_outputs <- function(node) {
  has_outputs <- length(node$outputs) > 0 && !is.null(node$outputs[[1]])

  if (has_outputs) {
    return(node$outputs[[1]])
  }
  character(0)
}

#' Format Outputs with Color Highlighting
#' @noRd
format_outputs_for_display <- function(outputs) {
  if (length(outputs) == 0) {
    return(format_column("<None>"))
  }
  # Highlight each output column
  format_list(outputs, format_column)
}

#' Format Dependencies with Color Highlighting
#' @noRd
format_dependencies_for_display <- function(dependencies) {
  if (length(dependencies) == 0) {
    return("None")
  }
  format_list(dependencies, format_column_ref)
}

#' @noRd
extract_node_dependencies <- function(node) {
  no_depend_cols <- length(node$depend_cols) == 0 ||
    is.null(node$depend_cols[[1]])

  if (no_depend_cols) {
    return(character(0))
  }

  depend_data <- node$depend_cols[[1]]
  is_valid_dataframe <- is.data.frame(depend_data) && nrow(depend_data) > 0
  if (is_valid_dataframe) {
    return(qualify_column_refs(depend_data))
  }

  character(0)
}

#' Generate Node Type Specific Analysis
#'
#' @param node A single row data.table representing one node
#' @param outputs Character vector of output column names
#' @return Character string with probable cause
#' @noRd
generate_node_type_analysis <- function(node, outputs) {
  is_col_compute <- node$type == "col_compute"

  if (is_col_compute) {
    if (length(outputs) > 0) {
      outputs_str <- format_list(outputs, format_column)
    } else {
      outputs_str <- format_column("<None>")
    }
    return(paste0(
      "Component deriving ",
      outputs_str,
      " is not reachable from domain initialization"
    ))
  }

  # Default case for unexpected node types - state the fact
  paste(
    "Node type '{.code",
    node$type,
    "}' is not reachable from domain initialization"
  )
}
