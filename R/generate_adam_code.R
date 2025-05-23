#' Generates the complete set of ADaM programs
#' @param path_ui_data
#' @param path_std_lib
#' @param path_domain_keys
#' @param path_output
#' @param data_connection
#' @param check_cross_domain_adam_dependencies
#'
#' @return
#' @export
#'
#' @examples
#' @import data.table
generate_adam_code <- function(path_ui_data,
                               code_component_source_pkgs = NULL,
                               code_component_source_files = NULL,
                               path_trial_metadata,
                               path_domain_keys,
                               path_output,
                               data_connection = c("connector", "pharmaverse"),
                               check_cross_domain_adam_dependencies = TRUE) {

  # Read data from UI containing explicit user input
  ui_yml <- read_adam_specs(path_ui_data)
  ui_init <- purrr::list_transpose(ui_yml)[["init"]]
  trial_metadata <- yaml::read_yaml(path_trial_metadata)
  ui_table <- convert_yml_to_data_table(ui_yml)

  # Create a consolidated environment for code components
  unique_code_ids <- ui_table[!is.na(code_id) & !duplicated(code_id), code_id]
  code_component_env <- create_consolidated_env(
    packages = code_component_source_pkgs,
    source_files = code_component_source_files,
    code_ids = unique_code_ids
  )

  domain_keys <- yaml::read_yaml(path_domain_keys)

  # Combine UI data and standard components
  nodes_1 <- parse_code_components_metadata(pkgs = code_component_source_pkgs,
                                            source_files = code_component_source_files,
                                            function_names = unique_code_ids) |>
    update_ui_data(ui_table) |>
    add_node_id_fast()

  # Check that outputs are valid
  assert_valid_outputs(nodes_1)

  # Assign action types col_copy, col_echo and col_mutate
  nodes_2 <- assign_predecessor_action_types(nodes_1)

  # Enrich depend_cols.
  # - For external col_echo actions: include foreign keys
  # - For col_compute actions that inputs a core column and return the same
  #   column in the ADaM domain: Add output columns from all other actions that
  #   have the same core column as input
  # - replace "core" with actual domains
  nodes_3 <- update_depend_cols(nodes_2, domain_keys, ui_init)

  # Check ADaM column dependencies
  assert_valid_adam_dependencies(nodes_3, ui_init, domain_keys, check_cross_domain_adam_dependencies)

  # Create an initialize action per domain that:
  #   - absorb col_copy actions
  #   - change domains of depend cols from core domain(s) to ADaM domain
  nodes_4 <- create_domain_initialize_nodes(nodes_3, ui_init)

  # Identify edges in the topology graph
  edges <- make_edges(nodes_4)

  # Identify topological order of actions
  nodes_topo_order <- weighted_node_topo_sort(edges, nodes_4, primary_domain = "ADSL")

  # Group actions into programs that can be run as batches in a sequence
  program_sequence_1 <- group_nodes_optimal(nodes_topo_order, nodes_4, edges)

  # Add initialization actions to the program sequence
  program_sequence_2 <- add_program_init_nodes(program_sequence_1, nodes_4)

  # Add action to import external dependencies to the program sequence
  program_sequence_3 <- add_nodes_to_load_external_data(program_sequence_2, nodes_4, ui_init, domain_keys) |>
    add_node_to_write_data()

  # Create programs
  data_connection <- match.arg(data_connection)
  programs <- generate_program(
    program_sequence_3,
    nodes_4,
    domain_keys,
    code_component_env,
    trial_metadata,
    ui_yml,
    data_connection,
    path_output = path_output
  )

  return(
    list(
      programs = programs,
      program_sequence = program_sequence_3,
      edges = edges,
      data_for_visualization = program_sequence_1,
      data_model = nodes_4
    )
  )
}
