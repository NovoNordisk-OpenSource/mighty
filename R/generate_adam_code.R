#' Generates the complete set of ADaM programs
#'
#' @param path_ui_data
#' @param path_output
#' @param check_cross_domain_adam_dependencies
#' @param code_component_source_pkgs
#' @param code_component_source_files
#' @param path_trial_metadata
#'
#' @return
#' @export
#' @import data.table
#' @examples
generate_adam_code <- function(path_ui_data,
                               code_component_source_pkgs = NULL,
                               code_component_source_files = NULL,
                               path_trial_metadata,
                               path_output,
                               check_cross_domain_adam_dependencies = TRUE) {

  # Read data from UI containing explicit user input
  ui_yml <- read_adam_specs(path_ui_data)
  ui_init <- purrr::list_transpose(ui_yml)[["init"]]
  trial_metadata <- yaml::read_yaml(path_trial_metadata) |> assert_valid_trial_config()

  # Prepare the initial internal nodes data model and create environment to store
  # standard components
  actions_configuration <- setup_actions(
    ui_yml = ui_yml,
    code_component_source_pkgs = code_component_source_pkgs,
    code_component_source_files = code_component_source_files
  )


  domain_keys  <- collate_primary_keys(trial_metadata)

  actions <- processing_actions(
    actions_configuration$actions,
    domain_keys = domain_keys,
    ui_init = ui_init,
    check_cross_domain_adam_dependencies = check_cross_domain_adam_dependencies
  )

  # Identify edges in the topology graph
  edges <- make_edges(actions)

  program_sequence <- make_program_sequence(
    nodes = actions,
    edges = edges,
    ui_init = ui_init,
    domain_keys = domain_keys
  )

  # Create programs
  programs <- generate_program(
    program_sequence,
    actions,
    domain_keys,
    actions_configuration$code_component_env,
    trial_metadata,
    ui_yml,
    path_output = path_output
  )

  return(
    list(
      programs = programs,
      program_sequence = program_sequence,
      edges = edges,
      data_model = actions
    )
  )
}
