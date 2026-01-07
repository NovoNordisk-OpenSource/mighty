#' Sort actions according to topological order and dependencies
#'
#' @param edges A data frame representing edges in the directed graph with two columns (from, to).
#' @param nodes A data frame representing nodes including their identifiers and associated domains.
#' @param primary_domain A string representing the primary domain used for prioritization.
#'
#' @return A character vector of node IDs sorted in topological order.
#' @noRd
sort_actions <- function(edges, nodes, primary_domain = "ADSL") {
  # Initialize graph and supporting data structures
  graph_data <- initialize_graph_data(edges, nodes, primary_domain)

  # Main topological sort with priority-based selection
  sorted_nodes <- perform_topological_sort(graph_data, primary_domain)

  return(sorted_nodes)
}

#' Summarize Adjacency Matrix Dependencies
#'
#' @description
#' Converts an adjacency matrix to a data frame summary of dependencies,
#' filtering out zero and self-dependencies.
#'
#' @param adj_matrix Adjacency matrix to summarize.
#'
#' @return
#' Data frame with columns Var1, Var2, and value representing non-zero
#' dependencies between distinct nodes.
#'
#' @noRd
summarize_adjacency_matrix <- function(adj_matrix) {
  adj_matrix |>
    as.table() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::group_by(Var1, Var2) |>
    dplyr::summarize(value = sum(Freq), .groups = "drop") |>
    dplyr::filter(value > 0 & Var1 != Var2)
}

#' Initialize All Graph Data Structures Needed for Sorting
#'
#' @description
#' Creates and initializes graph data structures from edges and nodes to support
#' topological sorting and dependency analysis for domain processing.
#'
#' @details
#' This function builds comprehensive graph representations including:
#' - Directed graph from edge/node data
#' - Adjacency matrix for dependency relationships
#' - Domain lookup table for node-to-domain mapping
#' - Summary of inter-domain dependencies
#' - Identification of domains with upstream dependencies
#' - Special handling for primary domain dependencies
#'
#' @param edges Data frame containing edge relationships between nodes with
#'   columns for source and target nodes.
#' @param nodes Data frame containing node information with columns node_id
#'   and domain.
#' @param primary_domain Character string specifying the primary domain name
#'   for special dependency analysis.
#'
#' @return
#' List containing initialized graph data structures:
#' \itemize{
#'   \item adj_matrix: Adjacency matrix representation of the graph
#'   \item lkp_dom: Named vector mapping node IDs to domain names
#'   \item adj_matrix_summary: Summary of dependencies between nodes
#'   \item domains_upstr_dep: Domains with upstream dependencies
#'   \item nodes_upstr_dep_prim_dom: Nodes with upstream dependency on primary domain
#'   \item domains_upstr_dep_prim_dom: Domains with upstream dependency on primary domain
#' }
#'
#' @noRd
initialize_graph_data <- function(edges, nodes, primary_domain) {
  # Create directed graph
  g <- igraph::graph_from_data_frame(
    edges,
    directed = TRUE,
    vertices = nodes[, .(node_id, domain)]
  )

  # Create adjacency matrix
  adj_matrix <- igraph::as_adjacency_matrix(g, sparse = FALSE)

  # Create domain lookup table
  node_names <- attr(adj_matrix, "dimnames")[[1]]
  lkp_dom <- setNames(
    nodes$domain[match(node_names, nodes$node_id)],
    node_names
  )

  # Summarize dependencies between nodes from adjacent matrix
  adj_matrix_summary <- summarize_adjacency_matrix(adj_matrix)

  # Identify domains with upstream dependencies
  domains_upstr_dep <- adj_matrix_summary |>
    dplyr::filter(lkp_dom[Var1] != lkp_dom[Var2]) |>
    dplyr::mutate(domain = lkp_dom[Var2]) |>
    dplyr::select(domain) |>
    unlist() |>
    unname()

  # Identify nodes with upstream dependency on primary domain
  nodes_upstr_dep_prim_dom <- adj_matrix_summary |>
    dplyr::filter(
      lkp_dom[Var1] == primary_domain & lkp_dom[Var2] != primary_domain
    ) |>
    dplyr::select(Var2) |>
    unique() |>
    unlist() |>
    unname()

  # Identify domains with upstream dependency on primary domain
  domains_upstr_dep_prim_dom <- unique(lkp_dom[nodes_upstr_dep_prim_dom])

  list(
    adj_matrix = adj_matrix,
    lkp_dom = lkp_dom,
    adj_matrix_summary = adj_matrix_summary,
    domains_upstr_dep = domains_upstr_dep,
    nodes_upstr_dep_prim_dom = nodes_upstr_dep_prim_dom,
    domains_upstr_dep_prim_dom = domains_upstr_dep_prim_dom
  )
}

#' Perform the Main Topological Sorting Algorithm
#'
#' @description
#' Executes topological sorting on a graph using priority-based node selection,
#' with special handling for primary domains and upstream dependencies.
#'
#' @param graph_data List containing adjacency matrix, domain lookup, and
#'   upstream dependency information.
#' @param primary_domain Character string specifying the primary domain for
#'   priority selection.
#'
#' @return
#' Character vector of node names in topologically sorted order.
#'
#' @noRd
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
      sorted_nodes <- c(sorted_nodes, selected_nodes)
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
      candidate_nodes,
      sorted_nodes,
      adj_matrix,
      lkp_dom,
      primary_domain,
      domains_upstr_dep,
      domains_upstr_dep_prim_dom
    )
  }

  return(sorted_nodes)
}

#' Remove Selected Nodes from Adjacency Matrix
#'
#' @description
#' Removes specified nodes from an adjacency matrix by eliminating their
#' corresponding rows and columns.
#'
#' @param adj_matrix Adjacency matrix with named rows and columns.
#' @param selected_nodes Character vector of node names to remove from the matrix.
#'
#' @return
#' Adjacency matrix with selected nodes removed, maintaining original structure
#' for remaining nodes.
#' @noRd
remove_selected_nodes <- function(adj_matrix, selected_nodes) {
  idx_to_remove <- which(colnames(adj_matrix) %in% selected_nodes)
  adj_matrix[-idx_to_remove, -idx_to_remove, drop = FALSE]
}

#' Find Nodes with Zero In-Degree (No Incoming Edges)
#'
#' @description
#' Identifies nodes in an adjacency matrix that have no incoming dependencies
#' by finding columns with zero sum.
#'
#' @param adj_matrix Adjacency matrix where rows represent source nodes and
#'   columns represent target nodes.
#'
#' @return
#' Character vector of node names that have zero in-degree (no dependencies).
#'
#' @noRd
find_zero_indegree_nodes <- function(adj_matrix) {
  names(which(colSums(adj_matrix) == 0))
}

#' Select Node Based on Priority Rules
#'
#' @description
#' Selects the next node to process from a set of candidate nodes using a
#' hierarchical priority system to ensure optimal processing order.
#'
#' @details
#' This function applies a three-tier priority system:
#' 1. **Priority 1**: Prefer nodes in the same domain as the last selected node
#'    or nodes in the primary domain
#' 2. **Priority 2**: Prefer nodes in domains where all upstream dependencies
#'    have been satisfied
#' 3. **Priority 3**: Fall back to alphabetical ordering as a tie-breaker
#'
#' @param candidate_nodes Character vector of node IDs that are eligible for
#'   selection (typically nodes with no remaining dependencies).
#' @param sorted_nodes Character vector of previously selected nodes in
#'   processing order.
#' @param adj_matrix Adjacency matrix representing the dependency graph
#'   between nodes.
#' @param lkp_dom Named vector mapping node IDs to their domain names.
#' @param primary_domain Character string specifying the primary domain name
#'   for priority consideration.
#' @param domains_upstr_dep Character vector of domains that have upstream
#'   dependencies on other domains.
#' @param domains_upstr_dep_prim_dom Character vector of domains that have
#'   upstream dependencies specifically on the primary domain.
#'
#' @return
#' Character string containing the selected node ID based on the priority
#' rules, guaranteed to return exactly one node from the candidate set.
#'
#' @noRd
select_node_by_priority <- function(
  candidate_nodes,
  sorted_nodes,
  adj_matrix,
  lkp_dom,
  primary_domain,
  domains_upstr_dep,
  domains_upstr_dep_prim_dom
) {
  # Priority 1: Same domain as last selected node or primary domain
  priority1_result <- apply_priority_1(
    candidate_nodes,
    sorted_nodes,
    lkp_dom,
    primary_domain
  )
  if (!is.null(priority1_result)) {
    return(priority1_result)
  }

  # Priority 2: Domain with covered upstream dependencies
  priority2_result <- apply_priority_2(
    candidate_nodes,
    adj_matrix,
    lkp_dom,
    primary_domain,
    domains_upstr_dep,
    domains_upstr_dep_prim_dom
  )
  if (!is.null(priority2_result)) {
    return(priority2_result)
  }

  # Priority 3: First node sorted alphabetically
  return(sort(candidate_nodes)[1])
}

#' Apply Priority 1: Select Nodes from Last Selected Node's Domain
#'
#' @description
#' Selects the first candidate node that belongs to the same domain as the
#' last selected node, or the primary domain if no nodes have been selected.
#'
#' @param candidate_nodes Vector of candidate node identifiers.
#' @param sorted_nodes Vector of previously selected nodes in order.
#' @param lkp_dom Named vector mapping node identifiers to their domains.
#' @param primary_domain Character string specifying the primary domain.
#'
#' @return
#' The first candidate node from the target domain, or NULL if no candidates
#' match the domain criteria.
#'
#' @noRd
apply_priority_1 <- function(
  candidate_nodes,
  sorted_nodes,
  lkp_dom,
  primary_domain
) {
  n_stack <- length(sorted_nodes)
  base_dom <- ifelse(
    n_stack > 0,
    lkp_dom[sorted_nodes[n_stack]],
    primary_domain
  )
  is_same_dom <- lkp_dom[candidate_nodes] == base_dom

  if (any(is_same_dom)) {
    return(sort(candidate_nodes[is_same_dom])[1])
  }

  return(NULL)
}

#' Apply Priority 2: Complex Domain Dependency Logic
#'
#' @description
#' Selects candidate nodes based on domain dependency coverage, prioritizing
#' nodes from domains whose upstream dependencies have been satisfied.
#'
#' @param candidate_nodes Vector of candidate node identifiers.
#' @param adj_matrix Adjacency matrix representing node dependencies.
#' @param lkp_dom Named vector mapping node identifiers to their domains.
#' @param primary_domain Character string specifying the primary domain.
#' @param domains_upstr_dep Vector of domains with upstream dependencies.
#' @param domains_upstr_dep_prim_dom Vector of domains with upstream dependencies
#'   on the primary domain.
#'
#' @return
#' Selected node identifier from candidates with covered dependencies, or NULL
#' if no candidates have satisfied dependencies. Uses sub-priorities when
#' multiple candidates are available.
#'
#' @noRd
apply_priority_2 <- function(
  candidate_nodes,
  adj_matrix,
  lkp_dom,
  primary_domain,
  domains_upstr_dep,
  domains_upstr_dep_prim_dom
) {
  # Find domains with remaining upstream dependencies
  domains_upstr_dep_rem <- find_remaining_upstream_dependencies(
    adj_matrix,
    lkp_dom
  )

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
    candidate_nodes_upstr_dep_cov,
    adj_matrix,
    lkp_dom,
    primary_domain,
    domains_upstr_dep,
    domains_upstr_dep_prim_dom
  ))
}

#' Find Domains That Still Have Remaining Upstream Dependencies
#'
#' @description
#' Identifies domains that have unresolved upstream dependencies by analyzing
#' a domain-level adjacency matrix derived from action dependencies.
#'
#' @param adj_matrix Adjacency matrix representing dependencies between actions.
#' @param lkp_dom Named vector mapping action names to their corresponding domains.
#'
#' @return
#' Character vector of domain names that still have upstream dependencies.
#'
#' @noRd
find_remaining_upstream_dependencies <- function(adj_matrix, lkp_dom) {
  # Create domain-level adjacency matrix
  adj_dom <- lkp_dom[attr(adj_matrix, "dimnames")[[1]]] |> unname()
  adj_matrix_dom <- adj_matrix
  attr(adj_matrix_dom, "dimnames") <- list(adj_dom, adj_dom)

  # Summarize by domain
  domains_deps_rem <- summarize_adjacency_matrix(adj_matrix_dom)

  unique(domains_deps_rem$Var2)
}

#' Apply Priority 2 Sub-priorities for Covered Candidates
#'
#' @description
#' Applies hierarchical sub-priorities to select from candidates with covered
#' dependencies: primary domain nodes, nodes with downstream dependencies to
#' primary domain, nodes with covered upstream dependencies, then sorted fallback.
#'
#' @param candidate_nodes_upstr_dep_cov Vector of candidate nodes with covered
#'   upstream dependencies.
#' @param adj_matrix Adjacency matrix representing node dependencies.
#' @param lkp_dom Named vector mapping node identifiers to their domains.
#' @param primary_domain Character string specifying the primary domain.
#' @param domains_upstr_dep Vector of domains with upstream dependencies.
#' @param domains_upstr_dep_prim_dom Vector of domains with upstream dependencies
#'   on the primary domain.
#'
#' @return
#' Selected node identifier based on sub-priority rules (2a-2d), with sorting
#' as tiebreaker.
#'
#' @noRd
apply_priority_2_subpriorities <- function(
  candidate_nodes_upstr_dep_cov,
  adj_matrix,
  lkp_dom,
  primary_domain,
  domains_upstr_dep,
  domains_upstr_dep_prim_dom
) {
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
  adj_matrix_dom <- adj_matrix
  attr(adj_matrix_dom, "dimnames") <- list(adj_dom, adj_dom)

  domains_deps_rem_df <- adj_matrix_dom |>
    as.table() |>
    as.data.frame(stringsAsFactors = FALSE) |>
    dplyr::group_by(.data$Var1, .data$Var2) |>
    dplyr::summarize(value = sum(.data$Freq), .groups = "drop") |>
    dplyr::filter(.data$value > 0 & .data$Var1 != .data$Var2)

  domains_sub <- domains_deps_rem_df |>
    dplyr::filter(
      .data$Var1 %in% domains_upstr_dep_cov & .data$Var2 == primary_domain
    )

  if (nrow(domains_sub) > 0) {
    idx <- domains_upstr_dep_cov %in% domains_sub$Var1
    return(sort(candidate_nodes_upstr_dep_cov[idx])[1])
  }

  # 2c: Node from domain with non-empty upstream dependency that is already covered
  domains_nonempty_upstr_dep_cov <- intersect(
    domains_upstr_dep_cov,
    domains_upstr_dep
  )
  is_eligible <- domains_upstr_dep_cov %in% domains_nonempty_upstr_dep_cov

  if (any(is_eligible)) {
    candidate_nodes_upstr_nonempty_dep_cov <- candidate_nodes_upstr_dep_cov[
      is_eligible
    ]

    if (length(candidate_nodes_upstr_nonempty_dep_cov) == 1) {
      return(candidate_nodes_upstr_nonempty_dep_cov)
    }

    # 2c1: Node from domains with upstream dependency to primary domain
    has_upstr_dep_on_prim_dom <- lkp_dom[
      candidate_nodes_upstr_nonempty_dep_cov
    ] %in%
      domains_upstr_dep_prim_dom

    if (any(has_upstr_dep_on_prim_dom)) {
      return(sort(candidate_nodes_upstr_nonempty_dep_cov[
        has_upstr_dep_on_prim_dom
      ])[1])
    } else {
      # 2c2: First node sorted by domain, node_id
      return(sort(candidate_nodes_upstr_nonempty_dep_cov)[1])
    }
  } else {
    # 2d: First node sorted by domain, node_id
    return(sort(candidate_nodes_upstr_dep_cov)[1])
  }
}
