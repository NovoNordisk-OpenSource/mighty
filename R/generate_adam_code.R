#' Generates the complete set of ADaM programs
#'
#' @param path_ui_data
#' @param path_trial
#' @param check_cross_domain_adam_dependencies
#' @param path_trial_metadata
#'
#' @return
#' @export
#' @import data.table
#' @examples
generate_adam_code <- function(
  path_ui_data,
  standards_lib = NULL,
  path_trial_metadata,
  path_trial,
  check_cross_domain_adam_dependencies = TRUE
) {

  # Read data from UI containing explicit user input
  ui_yml <- read_adam_specs(path_ui_data, validate = TRUE)
  ui_init <- purrr::list_transpose(ui_yml)[["init"]]
  trial_metadata <- yaml::read_yaml(path_trial_metadata) |>
    assert_valid_trial_config()
  domain_keys <- collate_primary_keys(trial_metadata)

  # Prepare the initial internal nodes data model and create environment to store
  # standard components
  actions_configuration <- setup_actions(
    ui_yml = ui_yml,
    standards_lib = standards_lib,
    check_cross_domain_adam_dependencies,
    domain_keys
  )

  actions_01_base <- actions_configuration$actions
  actions_02_init <- add_initialize_domain_actions(actions_01_base, ui_init)
  actions_03_filter <- add_filter_domain_actions(
    actions_02_init,
    ui_init,
    domain_keys
  )
  edges <- make_edges(actions_03_filter)
  actions_04_org <- organize_actions(actions_03_filter, edges)
  actions_05_read <- add_read_data_actions(actions_04_org, ui_init)
  actions_06_write <- add_write_data_actions(actions_05_read)

  # Create programs
  actions_07_code <- render_code(
    actions = actions_06_write,
    domain_keys = domain_keys,
    ui_data = ui_yml,
    path_trial = path_trial
  )

  return(
    list(
      programs = compile_into_programs(actions_07_code),
      program_sequence = actions_07_code,
      edges = edges,
      data_model = actions_configuration
    )
  )
}
