#' Replace "core" dependencies with domain initialization nodes
#' @description Adds new nodes representing the domain initialization steps, and
#' removes the corresponding nodes that represent the SDTM core variables
#' @param nodes
#' @param domain_init_data
#'
#' @return
#' @export
#'
#' @examples

create_domain_initialize_nodes <- function(nodes, domain_init_data) {
  nodes_split <- split(nodes, by = "domain")

  # For each domain: Identify the core variables for copy/mutate nodes
  core_vars <- nodes_split |>
    lapply(extract_core_dependency_columns, domain_init_data = domain_init_data)

  # Create a domain_init action for each domain
  domain_init_nodes <- purrr::imap(lapply(core_vars, rbindlist),
                                   create_domain_init_node_i,
                                   nodes,
                                   domain_init_data) |>
    rbindlist()

  # Remove copy nodes because these are  absorbed  by domain_init nodes
  nodes_subset <- nodes[type!="copy", ]

  # mutate nodes are not absorbed by the domain_init nodes. However, the
  # dependencies specified in the mutate nodes should now point to the variables
  # outputted by the domain init node, not the original "core" domain(s).

  # The update is done by replacing the domain
  # name in the depend_cols with the ADaM domain name.

  nodes_subset <- replace_core_domain_with_adam_for_mutate_nodes(nodes_subset = nodes_subset, domain_init_data = domain_init_data)

  # Return the updated nodes with domain_init nodes
  return(rbind(nodes_subset, domain_init_nodes))
}


#' Extract dependency columns from "core" datasets
#' @details
#' This function identifies and extracts column dependencies that come from core domains
#' for a given domain in scope. It filters out dependencies from non-core (external) domains
#' and returns only the dependencies that are from core domains.
#'
#' The function first identifies core domains for the domain in scope, then filters to
#' copy_mutate type nodes to extract their dependencies. It checks that dependencies
#' are either all from core domains or all from non-core domains, and returns only
#' the dependencies from core domains.
#'
#' @param x A data frame containing domain information, dependency columns, and operation types.
#'   Must contain columns: 'type', 'outputs', 'depend_cols', and 'domain'.
#' @param domain_init_data A list containing initialization data for domains, including
#'   which domains are considered "core" for each domain.
#'
#' @return A named list of data frames, where each element contains dependency column
#'   information from core domains. Names of the list correspond to the output names
#'   from copy_mutate operations. NULL dependencies (from non-core domains) are removed.
#'
extract_core_dependency_columns <- function(x, domain_init_data) {
  # Identify column dependencies for all predecessors
  core_domains <- id_core_domains_for_domain_in_scope(x, domain_init_data)

  x_sub <- x[x$type == "copy"|x$type == "mutate", ]
  dep_cols <- x_sub$depend_cols
  names(dep_cols) <- unlist(x_sub$outputs)

  # Check that all domains are either core domain(s) (internal predecessor)
  # or none (external predecessor) and ignore external predecessors
  dep_cols_core <- dep_cols |>
    lapply(filter_core_domain_dependencies, core_domains = core_domains)

  # Remove NULL values from the list. Null values arise when a node had
  # non-core dependencies
  null_index <-
    dep_cols_core |>
    lapply(is.null) |>
    unlist()
  out <- dep_cols_core[!null_index]
  return(out)
}



id_core_domains_for_domain_in_scope <- function(x, domain_init_data) {
  domain_i <- x$domain[[1]]
  core_domains <- domain_init_data[[domain_i]][["core_domains"]]
}


filter_core_domain_dependencies <- function(y, core_domains) {
  domain_match <- y$domain %in% core_domains
  if (!(all(domain_match) | all(!domain_match))) {
    # TODO: Why is this check needed?
    stop(paste(
      "Invalid domain(s)",
      paste0(setdiff(y$domain, core_domains), collapse = ','),
      "in predecessor node"
    ))
  }
  if (all(!domain_match)) {
    return(NULL)
  }
  y
}

create_domain_init_node_i <- function(core_vars_domain_i,
                                      nm,
                                      nodes,
                                      domain_init_data) {
  new_node_i <- data.table::data.table(matrix(ncol = ncol(nodes), nrow = 1)) |>
    data.table::setnames(names(nodes))

  new_node_i[, `:=`(
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "domain_init",
    depend_cols = list(core_vars_domain_i),
    outputs = list(unique(core_vars_domain_i$column_name))
  )][, node_id := paste0(domain, "-", "domain_init")]
}

#' Replace "core" domains with ADaM "self" domain for mutate nodes
#' @details
#' mutate nodes are not absorbed by the domain_init nodes. However, the
#' dependencies specified in the mutate nodes should now point to the variables
#' outputted by the domain init node, not the original "core" domain(s). The
#' update is done by replacing the domain name in the depend_cols with the ADaM
#' domain name.

#'
#' @param nodes_subset
#' @param domain_init_data
#'
#' @returns
replace_core_domain_with_adam_for_mutate_nodes <- function(nodes_subset, domain_init_data) {
  for (i in seq_len(nrow(nodes_subset))) {
    if (nodes_subset[i, type == "mutate"]) {
      dep_cols <- nodes_subset[["depend_cols"]][[i]]
      domain_i <- nodes_subset[["domain"]][[i]]
      indx_core_vars <- dep_cols$domain %in% domain_init_data[[domain_i]][["core_domains"]]

      # Only core predecessors are modified
      if (any(indx_core_vars)) {
        dep_cols[["domain"]][indx_core_vars] <- domain_i
        dep_cols[["domain_type"]][indx_core_vars] <- "adam"
        nodes_subset[["depend_cols"]][[i]] <- unique(dep_cols)
      }
    }
  }
  return(nodes_subset)
}
