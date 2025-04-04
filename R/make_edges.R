#' @title Make edges
#'
#' @param nodes
#' @param primary_domain
#'
#' @return
#' @export
#'
#' @examples
make_edges <- function(nodes, primary_domain = "adsl") {
  # This gives a data.table showing which columns each node depends on. But a
  # column is not the same as a "node" in this framework. So we will then have
  # to match each column to a node that produces that column. That will allow us
  # to ID the parent node
  parents_expanded <- nodes[, rbindlist(depend_cols), by = node_id][, .(node_id, full_name)] |>
    data.table::setnames(c("node_id", "parent_column"))
  parents_expanded[["node_id"]] <- parents_expanded[["node_id"]]
  parents_expanded[["parent_column"]] <- parents_expanded[["parent_column"]]

  # This is a list matching each node id to the columns it produces. We can use
  # this to match the column to the node in the parents_expanded data.table. We
  # exclude rows, because they cannot add NEW columns, they can only modify
  # existing columms. Otherwise we can get circular dependencies in the
  # topology.
  children_expanded <- nodes[type != "row", rbindlist(outputs_complete), by = node_id][, .(node_id, full_name)] |>
    data.table::setnames(c("node_id", "child_column"))
  children_expanded[["node_id"]] <- children_expanded[["node_id"]]
  children_expanded[["child_column"]] <- children_expanded[["child_column"]]

  edges <- data.table::merge.data.table(
    parents_expanded,
    children_expanded,
    by.x = "parent_column",
    by.y = "child_column",
    suffixes = c("", "_parent")
  ) |>
    setnames(c("consumes_variable", "node_id", "parent_node")) |>
    data.table::setcolorder(c("parent_node", "node_id", "consumes_variable")) |>
    data.table::setkey(node_id)

  # Need to explicitly add row dependencies as edges. In this framework the user
  # needs to manually define any row dependencies an action has
  if (nrow(nodes[!is.na(depend_rows)]) > 0) {
    new_row_edges <- nodes[!is.na(depend_rows), unlist(depend_rows), by=.(node_id,domain)]|>
      data.table::setnames(c( "node_id", "domain", "parent_node"))

    edges <- new_row_edges[,parent_node:= paste0(domain,".",parent_node)][,.(parent_node, node_id)] |>
      rbind(edges, fill = TRUE) |>
      setkey(node_id)
  }



  # Only return edges that are not reflective
  edges[node_id != parent_node]

}
