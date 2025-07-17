#' Title
#'
#' @param ordered_nodes
#' @param actions
#' @param edges
#'
#' @return
#' @export
#'
#' @examples
organize_actions <- function(actions, edges){

  # Identify topological order of actions
  ordered_actions <- weighted_node_topo_sort(edges, actions[actions$type != "col_copy",], primary_domain = "ADSL")

  # When edges has additional columns, things go wrong
  edges <- edges[,.(parent_node, node_id)]

  vertices <- actions[, .(node_id, domain, type)]

  # Permute domain init actions
  ordered_actions_perm <- permute_topo_order(ordered_actions, vertices)

  # Group actions for each permutation
  program_order_perm <- lapply(ordered_actions_perm, function(x) {
    group_nodes(x, vertices, edges)
  })

  # Determine minimum number of programs for each permutation
  n_programs <- lapply(program_order_perm, function(x) {
    max(x$program_id)
  }) |> unlist()

  # Re-create rank per program
  program_order_min <- program_order_perm[[which.min(n_programs)]]
  program_order_min[["order"]]

  program_order_min[, rank := 1:.N, by = program_id]

  # Join actions and optimal node grouping
  return(program_order_min[, c("node_id", "program_id", "rank")] |>
           dplyr::left_join(actions, by = "node_id"))
}
