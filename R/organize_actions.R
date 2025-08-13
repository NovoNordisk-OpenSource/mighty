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

  # Sort actions according to execution order, aiming to:
  #  - execute primary domain actions as early as possible
  #  - minimize the number of switches between domains
  actions_sorted <- sort_actions(edges, actions[actions$type != "col_copy",],
                                 primary_domain = "ADSL")

  # Group actions in ADaM programs and within each group refine the action order
  # so that depending actions are executed after each other. This will improve
  # clarity of action lineage in the generated programs
  edges <- edges[,.(parent_node, node_id)]
  vertices <- actions[, .(node_id, domain, type)]
  actions_grouped <- group_actions(actions_sorted, vertices, edges)

  # Re-create rank per program to show the execution order of actions
  actions_grouped[, rank := 1:.N, by = program_id]

  # Join actions and optimal node grouping
  return(actions_grouped[, c("node_id", "program_id", "rank")] |>
           dplyr::left_join(actions, by = "node_id"))
}
