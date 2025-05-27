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
                               data_connection = c("connector", "pharmaverse", "custom_data"),
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
  # - For col_compute actions that input a core column and return the same
  #   column in the ADaM domain: Add output columns from all other actions that
  #   have the same core column as input
  nodes_3 <- update_depend_cols(nodes_2, domain_keys, ui_init)

  # Create an initialize action per domain that absorbs col_copy action
  nodes_4 <- create_domain_initialize_nodes(nodes_3)

  # Replace "core" with relevant domains
  nodes_5 <- replace_core_with_named_domain(nodes_4, ui_init)

  # Check ADaM column dependencies
  assert_valid_adam_dependencies(nodes_5, ui_init, domain_keys, check_cross_domain_adam_dependencies)

  # Identify edges in the topology graph
  edges <- make_edges(nodes_5)

  # Identify topological order of actions
  nodes_topo_order <- weighted_node_topo_sort(edges, nodes_5, primary_domain = "ADSL")

  # Group actions into programs that can be run as batches in a sequence
  program_sequence_1 <- group_nodes_optimal(nodes_topo_order, nodes_5, edges)

  # Add initialization actions to the program sequence
  program_sequence_2 <- add_program_init_nodes(program_sequence_1, nodes_5)

  # Add action to import external dependencies to the program sequence
  program_sequence_3 <- add_nodes_to_load_external_data(program_sequence_2, nodes_5, ui_init, domain_keys) |>
    add_node_to_write_data()

  # Create programs
  data_connection <- match.arg(data_connection)
  programs <- generate_program(
    program_sequence_3,
    nodes_5,
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
      data_model = nodes_5
    )
  )
}
