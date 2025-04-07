#' make_adam_program
#'
#' @param path_ui_data
#' @param path_std_lib
#' @param path_domain_keys
#' @param path_output
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
make_adam_program <- function(path_ui_data,
                              path_std_lib,
                              path_domain_keys,
                              path_output,
                              data_connection = c("connector", "pharmaverse")) {

  # Data from UI containing explicit user input
  ui_data_1 <- path_ui_data |>
    merge_yaml_files() |>
    parse_ui_data()

  # Enrich derivations in UI data with associated metadata from standard nodes
  ui_data_2 <- path_std_lib |>
    lapply(parse_node_metadata) |>
    unlist(recursive = FALSE) |>
    update_ui_data(ui_data_1)

  # Convert UI data with metadata to a data.table
  nodes <- convert_node_list_to_dt(ui_data_2$nodes)

  # Enrich predecessors in UI data with auto-generated metadata
  nodes_2 <- update_predecessors(nodes, path_domain_keys)

  # Enrich UI data with predecessors that are not stated in the UI data and that
  # are required for the derivations to be run
  nodes_3 <- add_implied_predecessors(nodes_2)

  # Create an initialization action per domain that consumes predecessors
  nodes_4 <- create_domain_initialize_nodes(nodes_3, ui_data_2$init)

  # Add information about external dependencies to the initialization action
  nodes_5 <- enrich_with_external_dependencies(nodes_4, ui_data_2$init)

  # Identify the edges in the topology
  edges <- make_edges(nodes_5)

  # Identify topological order
  nodes_topo_order <- weighted_node_topo_sort(edges, nodes_5, primary_domain = "adsl")

  # Group actions into programs that can be run as batches in a sequence
  program_sequence <- group_nodes_optimal(nodes_topo_order, nodes_5, edges)

  # Add initialization actions to the program sequence
  program_sequence_2 <- add_program_init_nodes(program_sequence, nodes_5)

  # Add action to import external dependencies to the program sequence
  program_sequence_3 <- add_nodes_to_load_external_data(program_sequence_2, nodes_5)

  # Write the programs to the output path
  data_connection <- match.arg(data_connection)
  programs <- generate_program(
    program_sequence_3,
    nodes_5,
    yaml::read_yaml(path_domain_keys),
    path_std_lib,
    ui_data_2$trial_metadata,
    data_connection
  )
  write_adam_programs(programs, path_output)

  return(list(program_sequence = program_sequence_3,
              edges = edges,
              data_model = ui_data_2$nodes))
}
