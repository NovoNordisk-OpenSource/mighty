#' Group Actions by Domain for Program Generation
#'
#' This function groups ordered action nodes by their ADaM domain to create
#' logical program units. It processes a dependency-ordered sequence of nodes
#' and groups adjacent nodes from the same domain into individual programs,
#' while respecting cross-domain dependencies.
#'
#' @param ordered_nodes Character vector of node identifiers in dependency-resolved
#'   execution order. This should be a topologically sorted sequence where all
#'   dependencies of each node appear before the node itself.
#' @param vertices A data.table containing node metadata with required columns:
#'   \describe{
#'     \item{node_id}{Character. Unique identifier for each action node}
#'     \item{domain}{Character. ADaM domain name (e.g., "ADSL", "ADAE").
#'                   Missing values are treated as SDTM inputs}
#'     \item{type}{Character. Action type specification}
#'   }
#' @param edges A data.table defining dependencies between nodes with columns:
#'   \describe{
#'     \item{node_id}{Character. Source node identifier}
#'     \item{depends_on}{Character. Target node that this node depends on}
#'   }
#'
#' @return A data.table containing the grouped actions with columns:
#'   \describe{
#'     \item{node_id}{Character. Original node identifier}
#'     \item{domain}{Character. ADaM domain name}
#'     \item{node_group}{Integer. Sequential group number assigned during traversal}
#'     \item{node_level}{Integer. Hierarchical level within the dependency tree}
#'     \item{program_id}{Integer. Final program identifier grouping adjacent same-domain nodes}
#'     \item{rank}{Integer. Execution order within the overall sequence}
#'     \item{type}{Character. Action type from the vertices table}
#'   }
#'
#' @details
#' The grouping algorithm works as follows:
#' \enumerate{
#'   \item Assigns execution ranks to nodes based on the provided order
#'   \item Handles missing domain values by assigning them to "sdtm"
#'   \item Iteratively traverses the dependency tree, assigning group numbers
#'   \item Groups adjacent nodes with the same domain into single programs
#'   \item Maintains dependency order while optimizing for domain locality
#' }
#'
#' The function includes a safety limit of 1000 node groups to prevent infinite
#' loops in case of circular dependencies or other edge cases.
#'
#' @section Algorithm Details:
#' The traversal process uses [traverse_and_group_actions()] to:
#' \itemize{
#'   \item Process nodes in dependency order
#'   \item Assign hierarchical group numbers
#'   \item Track remaining unprocessed nodes
#'   \item Build the final program structure
#' }
#'
#' Adjacent nodes from the same domain are consolidated using `rleid()` to
#' create contiguous program blocks that minimize cross-program dependencies.
#'
#' @examples
#' \dontrun{
#' # Example vertices and edges
#' vertices <-  data.table(
#'   node_id = c("init_adsl", "derive_age", "init_adae", "derive_aesev"),
#'   domain = c("ADSL", "ADSL", "ADAE", "ADAE"),
#'   type = c("init", "derive", "init", "derive")
#' )
#'
#' edges <- data.table(
#'   node_id = c("derive_age", "init_adae", "derive_aesev"),
#'   depends_on = c("init_adsl", "init_adsl", "init_adae")
#' )
#'
#' ordered_nodes <- c("init_adsl", "derive_age", "init_adae", "derive_aesev")
#'
#' # Group actions by domain
#' grouped <- group_actions(ordered_nodes, vertices, edges)
#'
#' # View program groupings
#' grouped[, .(nodes = paste(node_id, collapse = ", ")), by = program_id]
#' }
#'
#' @seealso
#' [traverse_and_group_actions()] for the core traversal algorithm,
#' [organize_actions()] for the broader action organization workflow,
#' [make_edges()] for creating dependency edges
#'
#' @export
group_actions <- function(ordered_nodes, vertices, edges) {
  checkmate::assert_data_table(edges)
  checkmate::assert_data_table(vertices)
  n_remaining <- data.table(node_id = ordered_nodes)[, rank := .I] |>
    merge(vertices, by = "node_id", all.x = TRUE) |>
    setorder(rank)

  # TODO: how to handle SDTM inputs
  n_remaining[is.na(domain), domain := "sdtm"]

  n_stack <- data.table(
    domain = c(),
    node_i = c(),
    node_group = c(),
    node_level = c()
  )

  # Enrich edges with domain
  edges <- edges[
    n_remaining[, .(node_id, domain)],
    on = .(node_id),
    nomatch = NULL
  ]

  # Apply tree growth until depletion of nodes or node_group reaches 1000
  node_group <- 1
  while (nrow(n_remaining) > 0 && node_group <= 1000) {
    res <- traverse_and_group_actions(n_remaining, n_stack, edges, node_group)
    n_remaining <- res[["n_remaining"]]
    n_stack <- res[["n_stack"]]
    node_group <- node_group + 1
  }

  # Collect adjacent nodes of the same domain into a single program
  n_stack[, program_id := rleid(domain)]

  # Make rank explicit
  n_stack_1 <- n_stack[, rank := .I]

  # Merge "type" column back on
  out <- n_stack_1[
    vertices[, .(node_id, type)],
    on = .(node_id),
    nomatch = 0
  ] |>
    data.table::setorder(program_id, rank)
  return(out)
}



#' Traverse and Group Actions in Dependency Tree
#'
#' This function recursively traverses a dependency tree of action nodes within
#' a single domain, grouping them into execution levels while respecting parent-child
#' relationships. It processes nodes depth-first, ensuring all dependencies are
#' satisfied before executing child nodes.
#'
#' @param n_remaining A data.table of unprocessed nodes with required columns:
#'   \describe{
#'     \item{node_id}{Character. Unique identifier for each action node}
#'     \item{domain}{Character. ADaM domain name}
#'     \item{rank}{Integer. Original execution order ranking}
#'     \item{type}{Character. Action type (e.g., "col_*", "row_*")}
#'   }
#' @param n_stack A data.table accumulating processed nodes with columns:
#'   \describe{
#'     \item{domain}{Character. ADaM domain name}
#'     \item{node_id}{Character. Node identifier}
#'     \item{node_group}{Integer. Group number for this traversal}
#'     \item{node_level}{Integer. Hierarchical level in dependency tree}
#'   }
#' @param edges A data.table defining parent-child relationships with columns:
#'   \describe{
#'     \item{node_id}{Character. Child node identifier}
#'     \item{parent_node}{Character. Parent node identifier}
#'     \item{domain}{Character. Domain name (enriched from vertices)}
#'   }
#' @param node_group Integer. Current group number being assigned to nodes
#'   in this traversal iteration.
#' @param node_id_i Character. Current node being processed. If `NA` (default),
#'   starts with the first node in `n_remaining`.
#' @param domain_i Character. Current domain being processed. If `NA` (default),
#'   uses domain of the first node in `n_remaining`.
#' @param node_level_i Integer. Current hierarchical level. If `NA` (default),
#'   starts at level 0 for root nodes.
#'
#' @return A named list containing the updated state after traversal:
#'   \describe{
#'     \item{n_remaining}{Updated data.table with processed nodes removed}
#'     \item{n_stack}{Updated data.table with newly processed nodes added}
#'     \item{edges}{Original edges data.table (unchanged)}
#'     \item{node_group}{Original node_group number (unchanged)}
#'   }
#'
#' @details
#' The traversal algorithm follows these steps:
#' \enumerate{
#'   \item Initialize with the first unprocessed node if no starting node specified
#'   \item Remove current node from remaining nodes and add to processed stack
#'   \item Identify child candidates within the same domain
#'   \item Filter children to only those with all parents already processed
#'   \item Recursively process each accepted child in priority order
#'   \item Return when no more children can be processed
#' }
#'
#' @section Child Processing Priority:
#' When multiple children are available, they are processed in this order:
#' \enumerate{
#'   \item Column actions (type starting with "col") before row actions
#'   \item Within the same action type, by original rank order
#' }
#'
#' This ensures that column derivations are completed before row-level operations
#' that may depend on them.
#'
#' @section Recursion Control:
#' The function uses depth-first recursion within a single domain and node group.
#' It stops recursion when no child nodes have all their dependencies satisfied,
#' allowing the parent [group_actions()] function to start a new group if needed.
#'
#' @examples
#' \dontrun{
#' # Example setup
#' n_remaining <-  data.table(
#'   node_id = c("init_adsl", "derive_age", "derive_bmi"),
#'   domain = c("ADSL", "ADSL", "ADSL"),
#'   rank = 1:3,
#'   type = c("init", "col_derive", "col_derive")
#' )
#'
#' n_stack <- data.table(
#'   domain = character(0),
#'   node_id = character(0),
#'   node_group = integer(0),
#'   node_level = integer(0)
#' )
#'
#' edges <- data.table(
#'   node_id = c("derive_age", "derive_bmi"),
#'   parent_node = c("init_adsl", "init_adsl"),
#'   domain = c("ADSL", "ADSL")
#' )
#'
#' # Traverse and group within domain
#' result <- traverse_and_group_actions(
#'   n_remaining = n_remaining,
#'   n_stack = n_stack,
#'   edges = edges,
#'   node_group = 1
#' )
#'
#' # Check processed nodes
#' print(result$n_stack)
#' }
#'
#' @seealso
#' [group_actions()] for the parent function that orchestrates multiple traversals,
#' [make_edges()] for creating the dependency edge structure,
#' [organize_actions()] for the broader action organization workflow
#'
traverse_and_group_actions <- function(
  n_remaining,
  n_stack,
  edges,
  node_group,
  node_id_i = NA,
  domain_i = NA,
  node_level_i = NA
) {
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
  child_candidates <- edges[
    idx_node_remaining &
      domain == domain_i &
      parent_node == node_id_i,
    .(node_id)
  ]

  # Children with at least one unprocessed parent
  child_unprocessed_parents <- edges[
    idx_node_remaining &
      parent_node %in% n_remaining[["node_id"]],
    .(node_id)
  ]

  # Accept child candidates with no unprocessed parents
  node_id_children_accepted <- setdiff(
    child_candidates$node_id,
    child_unprocessed_parents$node_id
  )

  # Recursively apply function on each accepted child. Once algorithm reaches a
  # place where there are no accepted children available, it returns
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
      n_remaining[node_id %in% node_id_children_accepted][
        order(type, rank),
        node_id
      ]
  }
  for (j in node_id_children_accepted) {
    res <- traverse_and_group_actions(
      n_remaining,
      n_stack,
      edges,
      node_group,
      j,
      domain_i,
      node_level_i + 1
    )
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
