#' Group nodes based on domain
#'
#' @param ordered_nodes
#' @param vertices
#' @param edges
#'
#' @return
#' @export
#'
#' @examples
group_actions <- function(ordered_nodes, vertices, edges) {
  checkmate::assert_data_table(edges)
  checkmate::assert_data_table(vertices)
  n_remaining <- data.table(node_id = ordered_nodes)[, rank := .I] |>
    merge(vertices, by = "node_id", all.x = TRUE) |>
    setorder(rank)

  # TODO: how to handle SDTM inputs
  n_remaining[is.na(domain), domain := "sdtm"]

  n_stack <- data.table(domain = c(),
                        node_i = c(),
                        node_group = c(),
                        node_level = c())

  # Enrich edges with domain
  edges <- edges[n_remaining[,.(node_id, domain)], on=.(node_id), nomatch = NULL]

  # Apply tree growth until depletion of nodes or node_group reaches 1000
  node_group <- 1
  while (nrow(n_remaining) > 0 & node_group <= 1000) {
    res <- traverse_and_group_actions(n_remaining, n_stack, edges, node_group)
    n_remaining <- res[["n_remaining"]]
    n_stack <- res[["n_stack"]]
    node_group <- node_group + 1
  }

  # Collect adjacent nodes of the same domain into a single program
  n_stack[, program_id := rleid(domain)]

  # Make rank explicit
  n_stack_1 <- n_stack[, rank:= .I]

  # Merge "type" column back on
  out <- n_stack_1[vertices[,.(node_id, type)], on=.(node_id),
                   nomatch=0] |>
    data.table::setorder(program_id, rank)
  return(out)
}



#' Title
#'
#' @param node_group
#' @param node_id_i
#' @param edges
#' @param n_remaining
#' @param n_stack
#' @param domain_i
#'
#' @return
#' @export
#'
#' @examples
traverse_and_group_actions <- function(n_remaining,
                                     n_stack,
                                     edges,
                                     node_group,
                                     node_id_i = NA,
                                     domain_i = NA,
                                     node_level_i = NA) {
  checkmate::assert_data_table(n_remaining)
  checkmate::assert_data_table(n_stack)
  checkmate::assert_data_table(edges)
  # Initialize node pointer
  if (is.na(node_id_i)) {
    node_id_i <- n_remaining[["node_id"]][[1]]

    # Identify domain
    domain_i <- n_remaining[["domain"]][[1]]

    # First level
    node_level_i <- 0
  }

  # Update N_remaining
  n_remaining <- n_remaining[node_id != node_id_i, ]

  # Update N_stack
  n_stack <- rbind(
    n_stack,
    data.table(
      "domain" = domain_i,
      "node_id" = node_id_i,
      "node_group" = node_group,
      "node_level" = node_level_i
    )
  )

  # Identify ids of remaining nodes
  idx_node_remaining <- edges[["node_id"]] %in% n_remaining[["node_id"]]

  # Child candidates
  child_candidates <- edges[idx_node_remaining &
                              domain == domain_i &
                              parent_node == node_id_i, .(node_id)]

  # Children with at least one unprocessed parent
  child_unprocessed_parents <- edges[idx_node_remaining &
                                       parent_node %in% n_remaining[["node_id"]], .(node_id)]

  # Accept child candidates with no unprocessed parents
  node_id_children_accepted <- setdiff(child_candidates$node_id, child_unprocessed_parents$node_id)

  # Recursively apply function on each accepted child. Once algo reaches a place
  # where there are no accepted children available, it returns
  len_children <- length(node_id_children_accepted)
  if (len_children == 0) {
    return(
      list(
        "n_remaining" = n_remaining,
        "n_stack" = n_stack,
        "edges" = edges,
        "node_group" = node_group
      )
    )
  }

  if (len_children > 1) {
    # order children by type (column first), then naive rank. This ensures
    # that column nodes are executed before row nodes
    node_id_children_accepted <-
      n_remaining[node_id %in% node_id_children_accepted][order(type, rank),
                                                          node_id]
  }
  for (j in node_id_children_accepted) {
    res <- traverse_and_group_actions(n_remaining, n_stack, edges, node_group, j, domain_i, node_level_i+1)
    n_remaining <- res[["n_remaining"]]
    n_stack <- res[["n_stack"]]
  }

  return(
    list(
      "n_remaining" = n_remaining,
      "n_stack" = n_stack,
      "edges" = edges,
      "node_group" = node_group
    )
  )


}
