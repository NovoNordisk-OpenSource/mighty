#' Sort actions according to topological order and dependencies
#'
#' @param edges A data frame representing edges in the directed graph with two columns (from, to).
#' @param nodes A data frame representing nodes including their identifiers and associated domains.
#' @param primary_domain A string representing the primary domain used for prioritization.
#'
#' @return A character vector of node IDs sorted in topological order.
#' @export
sort_actions <-  function(edges, nodes, primary_domain = "ADSL") {
  # Initialize graph and supporting data structures
  graph_data <- initialize_graph_data(edges, nodes, primary_domain)

  # Main topological sort with priority-based selection
  sorted_nodes <- perform_topological_sort(graph_data, primary_domain)

  return(sorted_nodes)
}

#' Initialize all graph data structures needed for sorting
initialize_graph_data <- function(edges, nodes, primary_domain) {
  # Create directed graph
  g <- igraph::graph_from_data_frame(edges,
                                     directed = TRUE,
                                     vertices = nodes[, .(node_id, domain)])

  # Create adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(g, sparse = FALSE)

  # Create domain lookup table
node_names <- attr(adj_matrix, "dimnames")[[1]]
lkp_dom <- setNames(nodes$domain[match(node_names, nodes$node_id)], node_names)

  # Summarize dependencies between nodes from adjacent matrix
  adj_matrix_summary <- adj_matrix |>
    as.table() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::group_by(Var1, Var2) |>
    dplyr::summarize(value = sum(Freq), .groups = 'drop') |>
    dplyr::filter(value > 0 & Var1 != Var2)

  # Identify domains with upstream dependencies
  domains_upstr_dep <-  adj_matrix_summary |>
    dplyr::filter(lkp_dom[Var1] != lkp_dom[Var2]) |>
    dplyr::mutate(domain = lkp_dom[Var2]) |>
    dplyr::select(domain) |>
    unlist() |>
    unname()

  # Identify nodes with upstream dependency on primary domain
  nodes_upstr_dep_prim_dom <-  adj_matrix_summary |>
    dplyr::filter(lkp_dom[Var1] == primary_domain & lkp_dom[Var2] != primary_domain) |>
    dplyr::select(Var2) |>
    unique() |>
    unlist() |>
    unname()

  # Identify domains with upstream dependency on primary domain
  domains_upstr_dep_prim_dom <-  unique(lkp_dom[nodes_upstr_dep_prim_dom])

  list(
    adj_matrix = adj_matrix,
    lkp_dom = lkp_dom,
    adj_matrix_summary = adj_matrix_summary,
    domains_upstr_dep = domains_upstr_dep,
    nodes_upstr_dep_prim_dom = nodes_upstr_dep_prim_dom,
    domains_upstr_dep_prim_dom = domains_upstr_dep_prim_dom
  )
}

#' Perform the main topological sorting algorithm
perform_topological_sort <- function(graph_data, primary_domain) {
  sorted_nodes <- c()
  selected_nodes <- c()
  adj_matrix <- graph_data$adj_matrix
  lkp_dom <- graph_data$lkp_dom
  domains_upstr_dep <- graph_data$domains_upstr_dep
  domains_upstr_dep_prim_dom <- graph_data$domains_upstr_dep_prim_dom

  # Main loop to process nodes in topological order
  while (nrow(adj_matrix) > 0) {

    # Update sorted nodes set when there are selected nodes
    if (length(selected_nodes) > 0) {
      sorted_nodes <-  c(sorted_nodes, selected_nodes)
      adj_matrix <- remove_selected_nodes(adj_matrix, selected_nodes)

      if (max(dim(adj_matrix)) == 0) {
        next
      }
    }

    # Find candidate nodes with no incoming edges
    candidate_nodes <- find_zero_indegree_nodes(adj_matrix)

    if (length(candidate_nodes) == 0) {
      stop("The graph contains a cycle.")
    }

    # Select best candidate using priority rules
    selected_nodes <- select_node_by_priority(
      candidate_nodes, sorted_nodes, adj_matrix, lkp_dom,
      primary_domain, domains_upstr_dep, domains_upstr_dep_prim_dom
    )
  }

  return(sorted_nodes)
}

#' Remove selected nodes from adjacency matrix
remove_selected_nodes <- function(adj_matrix, selected_nodes) {
  idx_to_remove <- which(colnames(adj_matrix) %in% selected_nodes)
  adj_matrix[-idx_to_remove, -idx_to_remove, drop = FALSE]
}

#' Find nodes with zero in-degree (no incoming edges)
find_zero_indegree_nodes <- function(adj_matrix) {
  names(which(colSums(adj_matrix) == 0))
}

#' Select node based on priority rules (exact replication of original logic)
select_node_by_priority <- function(candidate_nodes, sorted_nodes, adj_matrix,
                                   lkp_dom, primary_domain, domains_upstr_dep,
                                   domains_upstr_dep_prim_dom) {

  # Priority 1: Same domain as last selected node or primary domain
  priority1_result <- apply_priority_1(candidate_nodes, sorted_nodes, lkp_dom, primary_domain)
  if (!is.null(priority1_result)) {
    return(priority1_result)
  }

  # Priority 2: Domain with covered upstream dependencies
  priority2_result <- apply_priority_2(
    candidate_nodes, adj_matrix, lkp_dom, primary_domain,
    domains_upstr_dep, domains_upstr_dep_prim_dom
  )
  if (!is.null(priority2_result)) {
    return(priority2_result)
  }

  # Priority 3: First node sorted alphabetically
  return(sort(candidate_nodes)[1])
}

#' Apply Priority 1: Select nodes from last selected node's domain
apply_priority_1 <- function(candidate_nodes, sorted_nodes, lkp_dom, primary_domain) {
  n_stack <- length(sorted_nodes)
  base_dom <- ifelse(n_stack > 0, lkp_dom[sorted_nodes[n_stack]], primary_domain)
  is_same_dom <-  lkp_dom[candidate_nodes] == base_dom

  if (any(is_same_dom)) {
    return(sort(candidate_nodes[is_same_dom])[1])
  }

  return(NULL)
}

#' Apply Priority 2: Complex domain dependency logic
apply_priority_2 <- function(candidate_nodes, adj_matrix, lkp_dom, primary_domain,
                            domains_upstr_dep, domains_upstr_dep_prim_dom) {

  # Find domains with remaining upstream dependencies
  domains_upstr_dep_rem <- find_remaining_upstream_dependencies(adj_matrix, lkp_dom)

  # Filter to nodes from domains with covered dependencies
  is_covered <- !(lkp_dom[candidate_nodes] %in% domains_upstr_dep_rem)

  if (!any(is_covered)) {
    return(NULL)
  }

  candidate_nodes_upstr_dep_cov <- candidate_nodes[is_covered]

  if (length(candidate_nodes_upstr_dep_cov) == 1) {
    return(candidate_nodes_upstr_dep_cov)
  }

  # Apply sub-priorities for multiple covered candidates
  return(apply_priority_2_subpriorities(
    candidate_nodes_upstr_dep_cov, adj_matrix, lkp_dom, primary_domain,
    domains_upstr_dep, domains_upstr_dep_prim_dom
  ))
}

#' Find domains that still have remaining upstream dependencies
find_remaining_upstream_dependencies <- function(adj_matrix, lkp_dom) {
  # Create domain-level adjacency matrix
  adj_dom <- lkp_dom[attr(adj_matrix, "dimnames")[[1]]] |> unname()
  adj_matrix_dom <-  adj_matrix
  attr(adj_matrix_dom, "dimnames") <- list(adj_dom, adj_dom)

  # Summarize by domain
  domains_deps_rem <- adj_matrix_dom |>
    as.table() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::group_by(Var1, Var2) |>
    dplyr::summarize(value = sum(Freq), .groups = 'drop') |>
    dplyr::filter(value > 0 & Var1 != Var2)

  unique(domains_deps_rem$Var2)
}

#' Apply Priority 2 sub-priorities for covered candidates
apply_priority_2_subpriorities <-  function(candidate_nodes_upstr_dep_cov, adj_matrix,
                                          lkp_dom, primary_domain, domains_upstr_dep,
                                          domains_upstr_dep_prim_dom) {

  # 2a: Node from primary domain
  is_prim_dom_node <- lkp_dom[candidate_nodes_upstr_dep_cov] == primary_domain
  if (any(is_prim_dom_node)) {
    return(sort(candidate_nodes_upstr_dep_cov[is_prim_dom_node])[1])
  }

  # 2b: Node from domain with downstream dependency to primary domain
  domains_deps_rem <- find_remaining_upstream_dependencies(adj_matrix, lkp_dom)
  domains_upstr_dep_cov <- lkp_dom[candidate_nodes_upstr_dep_cov]

  # Create the domains_deps_rem data frame for filtering
  adj_dom <- lkp_dom[attr(adj_matrix, "dimnames")[[1]]] |> unname()
  adj_matrix_dom <-  adj_matrix
  attr(adj_matrix_dom, "dimnames") <- list(adj_dom, adj_dom)

  domains_deps_rem_df <- adj_matrix_dom |>
    as.table() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::group_by(Var1, Var2) |>
    dplyr::summarize(value = sum(Freq), .groups = 'drop') |>
    dplyr::filter(value > 0 & Var1 != Var2)

  domains_sub <-  domains_deps_rem_df |>
    dplyr::filter(Var1 %in% domains_upstr_dep_cov & Var2 == primary_domain)

  if (nrow(domains_sub) > 0) {
    idx <- domains_upstr_dep_cov %in% domains_sub$Var1
    return(sort(candidate_nodes_upstr_dep_cov[idx])[1])
  }

  # 2c: Node from domain with non-empty upstream dependency that is already covered
  domains_nonempty_upstr_dep_cov <- intersect(domains_upstr_dep_cov, domains_upstr_dep)
  is_eligible <- domains_upstr_dep_cov %in% domains_nonempty_upstr_dep_cov

  if (any(is_eligible)) {
    candidate_nodes_upstr_nonempty_dep_cov <- candidate_nodes_upstr_dep_cov[is_eligible]

    if (length(candidate_nodes_upstr_nonempty_dep_cov) == 1) {
      return(candidate_nodes_upstr_nonempty_dep_cov)
    }

    # 2c1: Node from domains with upstream dependency to primary domain
    has_upstr_dep_on_prim_dom <- lkp_dom[candidate_nodes_upstr_nonempty_dep_cov] %in%
      domains_upstr_dep_prim_dom

    if (any(has_upstr_dep_on_prim_dom)) {
      return(sort(candidate_nodes_upstr_nonempty_dep_cov[has_upstr_dep_on_prim_dom])[1])
    } else {
      # 2c2: First node sorted by domain, node_id
      return(sort(candidate_nodes_upstr_nonempty_dep_cov)[1])
    }
  } else {
    # 2d: First node sorted by domain, node_id
    return(sort(candidate_nodes_upstr_dep_cov)[1])
  }
}
