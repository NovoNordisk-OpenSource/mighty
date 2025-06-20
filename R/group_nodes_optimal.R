#' Title
#'
#' @param ordered_nodes
#' @param nodes
#' @param edges
#'
#' @return
#' @export
#'
#' @examples
group_nodes_optimal <- function(nodes, edges){

  # Identify topological order of actions
  ordered_nodes <- weighted_node_topo_sort(edges, nodes, primary_domain = "ADSL")

  # When edges has additional columns, things go wrong
  edges <- edges[,.(parent_node, node_id)]

  vertices <- nodes[, .(node_id, domain, type)]

  # Permute domain init nodes
  ordered_nodes_perm <- permute_topo_order(ordered_nodes, vertices)

  # Group nodes for each permutation
  program_order_perm <- lapply(ordered_nodes_perm, function(x) {
    group_nodes(x, vertices, edges)
  })

  # Determine minimum number of programs for each permutation
  n_programs <- lapply(program_order_perm, function(x) {
    max(x$program_id)
  }) |> unlist()

  # Choose node order permutation with smallest minimum number of programs
  program_order_min <- program_order_perm[[which.min(n_programs)]]

  # For now remove node_level as it is not finalized
  program_order_min <- program_order_min[, -"node_level"]

  # Return optimal node grouping
  return(program_order_min)
}
