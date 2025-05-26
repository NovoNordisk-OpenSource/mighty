#' Topological sort of a weighted directed graph
#'
#' @param edges
#' @param nodes
#' @param primary_domain
#'
#' @return
#' @export
#'
#' @examples
weighted_node_topo_sort <- function(edges, nodes, primary_domain = "ADSL") {

  # Create a directed graph from the edges and nodes
  g <- igraph::graph_from_data_frame(edges,
                                     directed = TRUE,
                                     vertices = nodes[, .(node_id, domain)])

  # Weight the primary domain
  starting_domain <- which(toupper(igraph::V(g)$domain) == toupper(primary_domain))
  igraph::V(g)$weight <- 0
  igraph::V(g)[starting_domain]$weight <- 1

  # Create a list to store the sorted order
  sorted_order <- c()

  # Create a copy of the graph's adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(g, sparse = FALSE)

  # Get node weights
  node_weights <- igraph::V(g)$weight
  names(node_weights) <- igraph::V(g)$name

  # Save original order of nodes for adding weights as attributes
  node_weights0 <- node_weights

  while (nrow(adj_matrix) > 0) {
    # Find nodes with no incoming edges
    available_nodes <- which(colSums(adj_matrix) == 0)

    # If no available nodes, the graph has a cycle
    if (length(available_nodes) == 0) {
      stop("The graph contains a cycle")
    }

    # Sort available nodes by weight (descending order)
    available_names <- names(node_weights)[available_nodes]
    sorted_level <- available_names[order(node_weights[available_nodes], decreasing = TRUE)]

    # Add sorted nodes to the result
    sorted_order <- c(sorted_order, sorted_level)

    # Remove processed nodes from the adjacency matrix and weights
    adj_matrix <- adj_matrix[-available_nodes, -available_nodes, drop = FALSE]
    node_weights <- node_weights[-available_nodes]
  }

  # Attach node weights as attributes
  attr(sorted_order, "weight") <- as.numeric(node_weights0[sorted_order])

  return(sorted_order)
}
