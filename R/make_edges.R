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
make_edges <-  function(nodes, primary_domain = "ADSL") {
  # Process column dependencies to create edges
  column_edges <- create_column_dependency_edges(nodes)

  # Process row dependencies if they exist
  row_edges <- create_row_dependency_edges(nodes)

  # Combine all edges and clean them up
  all_edges <- combine_and_clean_edges(column_edges, row_edges)

  return(all_edges)
}

#' @title Create edges based on column dependencies
#' @description Identifies parent-child relationships based on column
#'   dependencies
#' @param nodes The nodes data.table
#' @return A data.table of edges based on column dependencies
create_column_dependency_edges <- function(nodes) {
  # Get expanded parent columns that each node depends on
  parents_expanded <- expand_parent_columns(nodes)

  # Get expanded child columns that each node produces
  children_expanded <- expand_child_columns(nodes)

  # Merge to create edges
  edges <- data.table::merge.data.table(
    parents_expanded,
    children_expanded,
    by.x = c("parent_column", "parent_column_domain"),
    by.y = c("child_column", "child_column_domain"),
    suffixes = c("", "_parent")
  ) |>
    setnames(c("consumes_variable", "consumes_variable_domain", "node_id", "parent_node")) |>
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
  parents_expanded <- nodes[, rbindlist(depend_cols), by = .(node_id, domain)][,-"domain_type"] |>
    setnames(c("node_id", "domain", "parent_column", "parent_column_domain"))

  # Need to replace "core" with domain of node
  parents_expanded[parent_column_domain=="core", parent_column_domain:= domain]
  parents_expanded <-  parents_expanded[,.(node_id, parent_column, parent_column_domain)]

  return(parents_expanded)
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
      child_column_domain = x$domain
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
  # Check if any nodes have row dependencies
  has_row_dependencies <- nodes[!is.na(depend_rows)] |> nrow() > 0

  if (!has_row_dependencies) {
    return(NULL)
  }

  # Extract row dependencies
  new_row_edges <-  nodes[!is.na(depend_rows),
                          unlist(depend_rows),
                          by = .(node_id, domain)] |>
    data.table::setnames(c("node_id", "domain", "parent_node"))

  # Match code_ids to node_ids to create proper edges
  row_edges <-  nodes[, .(code_id, node_id, domain)] |>
    merge(new_row_edges,
          by.x = c("code_id", "domain"),
          by.y = c("parent_node", "domain"),
          suffixes = c("_parent", "")) |>
    data.table::setnames(old = "node_id_parent", "parent_node")

  return(row_edges)
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
