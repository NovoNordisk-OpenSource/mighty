#' Create Program Execution Sequence for ADaM Generation
#'
#' @description Organizes processed nodes into an optimal execution sequence by
#' grouping them into programs and adds "non-user" nodes, such as
#' initiailization nodes and "read" nodes
#'
#' @param nodes Data.table containing processed nodes from
#'   \code{\link{make_nodes}} with enriched dependencies, proper action types,
#'   and resolved domain references
#' @param edges Data.table containing directed edges between nodes representing
#'   dependencies, typically created by \code{\link{make_edges}}
#' @param ui_init List containing UI initialization data with domain-specific
#'   configurations, filters, and core domain mappings
#' @param domain_keys Named list mapping domain names to their respective
#'   primary key columns used for data merging and foreign key relationships
make_program_sequence <- function(nodes, edges, ui_init, domain_keys){
  # Group actions into programs that can be run as batches in a sequence
  program_sequence_1 <- group_nodes_optimal(nodes, edges) |>
    # Add initialize_domain actions to 'initial' programs
    add_initialize_domain_nodes() |>
    # Add read_domain actions to 'update' programs
    add_read_domain_nodes(nodes) |>
    # Add read_data actions to read all data sets required for each ADaM program
    add_read_data_nodes(nodes, ui_init, domain_keys) |>
    # Add action to save generated ADaM table
    add_write_domain_nodes()
}
