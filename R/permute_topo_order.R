#' Permute domain init nodes in topological order
#'
#' @param topo_order_names
#' @param vertices
#'
#' @return
#' @export
#'
#' @examples
permute_topo_order <- function(topo_order_names, vertices){

  # Names of domain init nodes
  init_node_id <- vertices[vertices$type == "preprocess_domain",][["node_id"]]

  # Indices of domain init nodes in topo_order_names
  init_nodes_idx <- which(topo_order_names %in% init_node_id)

  # Unique weight values of domain init nodes
  init_weight_vals <- unique(attr(topo_order_names, "weight")[init_nodes_idx])

  # Initialize list of node order permutations
  topo_order_perm <- list(as.character(topo_order_names))

  # Permute domain init nodes
  for(i in init_weight_vals){

    # Identify domain init nodes with weight i
    j <- intersect(which(attr(topo_order_names, "weight") == i),
                   init_nodes_idx)

    # Permute domain init nodes with weight i
    if(length(j) > 1){
      p <- combinat::permn(j)

      # Order permutations according to order of appearance
      comparable_structure <- function(x) {
        paste(x, collapse = "-")
      }
      comparable_elements <- sapply(p, comparable_structure)
      ordered_indices <- order(comparable_elements)
      p_ordered <- p[ordered_indices]

      # Create node order permutations
      topo_order_perm <- c(topo_order_perm, lapply(p_ordered, function(x) {
        if(!identical(x, j)){
          tn <- topo_order_names
          tn[j] <- tn[x]
          return(as.character(tn))
        }
      }))
    }
  }

  # Choose first node order permutation with smallest minimum number of programs
  is_null <- sapply(topo_order_perm, is.null)
  if(any(is_null)){
    topo_order_perm <- topo_order_perm[-which(is_null)]
  }

  return(topo_order_perm)
}
