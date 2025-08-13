#' @title Make edges between nodes in a computational graph
#' @description Creates directed edges between nodes based on their
#' dependencies. The function identifies parent-child relationships by matching
#' columns that nodes produce and consume.
#'
#' @details The function works by:
#' 1. Identifying which columns each node depends on
#' 2. Matching these columns to nodes that produce them
#' 3. Creating edges from producer nodes to consumer nodes
#' 4. Adding explicit row dependencies when specified
#' 5. Removing self-referential edges and duplicates
#'
#' @param nodes A data.table containing node definitions with columns: node_id,
#'   domain, type, depend_cols, depend_rows, outputs, and code_id
#' @param primary_domain Character string specifying the primary domain
#'   (default: "ADSL")
#'
#' @return A data.table with two columns (parent_node, node_id) representing the
#'   directed edges in the computational graph
#'
make_edges <- function(nodes, primary_domain = "ADSL") {

  # Process column dependencies to create edges
  column_edges <- create_column_dependency_edges(nodes)

  # Process row dependencies if they exist
  row_edges <- create_row_dependency_edges(nodes)

  # Remove column edges that point from filter_domain to actions that
  # filter_domain depends on. This is to avoid circular dependencies.
  column_edges_2 <- remove_child_filter_edges(column_edges, nodes)

  # Combine all edges and clean them up
  all_edges <- combine_and_clean_edges(column_edges_2, row_edges)

  return(all_edges)
}

#' @title Create edges based on column dependencies
#' @description Identifies parent-child relationships based on column
#'   dependencies
#' @param nodes The nodes data.table
#' @return A data.table of edges based on column dependencies
create_column_dependency_edges <- function(nodes) {

  nodes2 <- nodes[nodes$type != "col_copy",]

  # Get expanded parent columns that each node depends on
  parents_expanded <- expand_parent_columns(nodes2)

  # Get expanded child columns that each node produces
  children_expanded <- expand_child_columns(nodes2)

  # Merge to create edges
  edges <- data.table::merge.data.table(
    parents_expanded,
    children_expanded,
    by.x = c("parent_column", "parent_column_domain", "parent_column_domain_type"),
    by.y = c("child_column", "child_column_domain", "child_column_domain_type"),
    suffixes = c("", "_parent")
  ) |>
    setnames(c("consumes_variable", "consumes_variable_domain", "consumes_variable_domain_type","node_id", "parent_node")) |>
    data.table::setcolorder(c("parent_node", "node_id", "consumes_variable")) |>
    data.table::setkey(node_id)

  return(edges)
}

#' @title Expand parent columns for each node
#' @description Creates a detailed list of columns each node depends on
#' @param nodes The nodes data.table
#' @return A data.table with expanded parent column information
expand_parent_columns <-  function(nodes) {
  # This gives a data.table showing which columns each node depends on
  parents_expanded <- nodes[, rbindlist(depend_cols), by = .(node_id, domain)] |>
    setnames(c("node_id", "domain", "parent_column", "parent_column_domain", "parent_column_domain_type"))

  return(parents_expanded[,.(node_id, parent_column, parent_column_domain, parent_column_domain_type)])
}

#' @title Expand child columns for each node
#' @description Creates a detailed list of columns each node produces
#' @param nodes The nodes data.table
#' @return A data.table with expanded child column information
expand_child_columns <- function(nodes) {
  # This is a list matching each node id to the columns it produces
  # We exclude row_compute nodes as they only modify existing columns
  extract_child_columns <- function(x) {
    data.table::data.table(
      child_column = x$outputs[[1]],
      child_column_domain = x$domain,
      child_column_domain_type =
        ifelse(x$type == "preprocess_domain", "init", "adam")
    )
  }

  children_expanded <- nodes[type != "row_compute",
                             extract_child_columns(.SD),
                             by = node_id]

  return(children_expanded)
}

#' @title Create edges based on row dependencies
#' @description Identifies parent-child relationships based on explicit row
#'   dependencies
#' @param nodes The nodes data.table
#' @return A data.table of edges based on row dependencies, or NULL if none
#'   exist
create_row_dependency_edges <- function(nodes) {

  # Check if there are any row operations and exit early if not
  has_row_operations <- nodes[type == "row_compute"] |> nrow() > 0
  if (!has_row_operations) {
    return(NULL)
  }

  # Row dependencies come from 2 sources:
  #  1) depend_rows in any actions
  #  2) depend_cols in row actions that originates from init_domain or filter_domain

  # 1) Extract row dependencies from depend_rows
  new_row_edges1 <-
    if (any(!is.na(nodes$depend_rows))) {
      nodes[!is.na(depend_rows), unlist(depend_rows), by = .(node_id, domain)] |>
        data.table::setnames(c("node_id", "domain", "parent_node"))
    } else {
      data.table(node_id = character(0),
                 domain = character(0),
                 parent_node = character(0))
    }

  # 2) Extract dependencies to row actions from init_domain and filter_domain

  # Get row action dependencies
  parents_expanded <- expand_parent_columns(nodes[type == "row_compute",])

  # Get outputs of init_domain and filter_domain actions
  children_expanded <- expand_child_columns(nodes[type %in% c("init_domain","filter_domain")])

  # Join to identify edges
  new_row_edges2 <- data.table::merge.data.table(
    parents_expanded[,.(node_id, parent_column, parent_column_domain)],
    children_expanded[,.(node_id, child_column, child_column_domain)],
    by.x = c("parent_column", "parent_column_domain"),
    by.y = c("child_column", "child_column_domain"),
    suffixes = c("", "_parent")
  ) |>
    setnames(c("consumes_variable", "domain","node_id", "parent_node")) |>
    data.table::setcolorder(c("node_id", "domain", "parent_node")) |>
    data.table::setkey(node_id)
  new_row_edges2[, consumes_variable := NULL]

  # Match code_ids to node_ids to create proper edges
  row_edges <- nodes[, .(code_id, node_id, domain)] |>
    merge(rbind(new_row_edges1, new_row_edges2),
          by.x = c("code_id", "domain"),
          by.y = c("parent_node", "domain"),
          suffixes = c("_parent", "")) |>
    data.table::setnames(old = "node_id_parent", "parent_node")

  return(row_edges)
}

remove_child_filter_edges <- function(edges, actions) {

  # Split actions by ADaM domain
  actions_split <- split(actions, by = "domain")

  # Loop over each ADaM domain and update edges
  for (nm in names(actions_split)) {

    # Extract filter action
    filter_action <- actions_split[[nm]] |>
      dplyr::filter(type == "filter_domain")

    # Extract any other action
    other_actions <- actions_split[[nm]] |>
      dplyr::filter(type != "filter_domain")

    if (nrow(filter_action) > 0) {

      # Filter actions outputs
      filter_action_outputs <- filter_action$outputs[[1]]

      # Identify children of filter action that are other than col_copys
      # These children must be removed in the edges to avoid circular dependencies
      children_to_remove <- lapply(seq_len(nrow(other_actions)), function(i) {
        is_child_to_remove <- other_actions$type[[i]] != "col_copy" &
          other_actions$outputs[[i]] %in% filter_action_outputs |>
          any()
        if (is_child_to_remove) {
          return(other_actions$node_id[[i]])
        }
      }) |> unlist()

      # Remove edges from filter action to the identified children
      edges <- edges[!(edges$parent_node == filter_action$node_id & edges$node_id %in% children_to_remove)]

    }
  }

  return(edges)
}

#' @title Combine and clean all edges
#' @description Combines column and row dependency edges, removes
#'   self-references and duplicates
#' @param column_edges Edges from column dependencies
#' @param row_edges Edges from row dependencies
#' @return A cleaned data.table of unique edges
combine_and_clean_edges <-  function(column_edges, row_edges) {
  # Combine column and row edges if row edges exist
  if (!is.null(row_edges)) {
    all_edges <- rbind(column_edges, row_edges, fill = TRUE) |>
      setkey(node_id)
  } else {
    all_edges <- column_edges
  }

  # Remove edges that are reflective (self-references)
  filtered_edges <- all_edges[node_id != parent_node]

  # Only return unique edges
  unique_edges <- filtered_edges[, .SD[1], by = .(parent_node, node_id)][, .(parent_node, node_id)]

  return(unique_edges)
}
