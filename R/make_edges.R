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

  parents_expanded <- nodes[, rbindlist(depend_cols), by = .(node_id, domain)][,-"domain_type"] |>
    setnames(c("node_id", "domain", "parent_column", "parent_column_domain"))

  # Need to replace "core" with domain of node
  parents_expanded[parent_column_domain=="core", parent_column_domain:= domain]
  parents_expanded <- parents_expanded[,.(node_id, parent_column, parent_column_domain)]

  # This is a list matching each node id to the columns it produces. We can use
  # this to match the column to the node in the parents_expanded data.table. We
  # exclude rows, because they cannot add NEW columns, they can only modify
  # existing columms. Otherwise we can get circular dependencies in the
  # topology.


  fd <- function(x){
    data.table::data.table(
                           child_column = x$outputs[[1]],
                           child_column_domain = x$domain)
  }
  children_expanded <- nodes[type!="row", fd(.SD), by=node_id]


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

  # Need to explicitly add row dependencies as edges. In this framework the user
  # needs to manually define any row dependencies an action has
  has_row_dependencies <- nodes[!is.na(depend_rows)] |> nrow() > 0
  if (has_row_dependencies) {
    # Since rows have a unique code_id (for now), we can ID rows using simply
    # their code_ id. This might have to change in the future when we
    # parameterize, and potentially call the same code with different parameters
    # on multiple places
    new_row_edges <- nodes[!is.na(depend_rows), unlist(depend_rows), by=.(node_id,domain)]|>
      data.table::setnames(c( "node_id", "domain", "parent_node"))

    edges <- nodes[,.(code_id, node_id, domain)] |>
      merge(new_row_edges, by.x=c("code_id", "domain"), by.y=c("parent_node", "domain"), suffixes = c("_parent", "")) |>
      data.table::setnames(old = "node_id_parent", "parent_node") |>
      rbind(edges, fill = TRUE) |>
      setkey(node_id)
  }


  # Remove edges that are reflective
  edges2 <- edges[node_id != parent_node]

  # Only return unique edges
  edges2[, .SD[1], by = .(parent_node, node_id)][,. (parent_node, node_id)]

}
