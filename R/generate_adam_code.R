#' Generate Complete Set of ADaM Programs
#'
#' This function orchestrates the complete workflow for generating ADaM (Analysis
#' Data Model) programs from user interface specifications and trial metadata.
#' It processes UI data, validates dependencies, creates action sequences, and
#' renders executable R code for ADaM dataset creation.
#'
#' @param path_ui_data Character string. File path to the UI data specifications
#'   (typically a YAML file) containing user-defined ADaM dataset configurations,
#'   column definitions, and transformation rules.
#' @param standards_lib Optional list or environment containing standard code
#'   components and templates. If `NULL`, uses default standards library.
#' @param path_trial_metadata Character string. File path to the trial metadata
#'   YAML file containing study-specific configuration, domain definitions,
#'   and primary key specifications.
#' @param path_trial Character string. Base path to the trial directory where
#'   generated programs and data files will be referenced or stored.
#' @param check_cross_domain_adam_dependencies Logical. If `TRUE` (default),
#'   validates dependencies across different ADaM domains. If `FALSE`, only
#'   validates dependencies within individual domains.
#' @param data_context Optional list or environment providing additional context
#'   about available data sources for executable program generation. If `NULL`,
#'   all programs are considered potentially executable.
#'
#' @return A named list containing the complete ADaM program generation results:
#'   \describe{
#'     \item{programs}{List of complete, ready-to-execute R programs for each ADaM dataset}
#'     \item{program_sequence}{Detailed action sequence with rendered code for all programs}
#'     \item{executable_programs}{Subset of programs that can be executed with available data}
#'     \item{executable_program_sequence}{Action sequence for executable programs only}
#'     \item{edges}{Dependency graph edges showing relationships between actions}
#'     \item{actions}{Base action configuration before code rendering}
#'     \item{rendered_components}{Standard code components that were rendered during generation}
#'   }
#'
#' @details
#' The function executes the following workflow:
#' \enumerate{
#'   \item Reads and validates UI specifications and trial metadata
#'   \item Sets up initial action configurations with dependency validation
#'   \item Adds domain initialization, filtering, and data reading actions
#'   \item Creates dependency graph and organizes actions in execution order
#'   \item Adds data writing actions and checks executable status
#'   \item Renders R code for both complete and executable program sets
#'   \item Compiles individual actions into complete, runnable programs
#' }
#'
#' Each generated program includes all necessary data transformations, derivations,
#' and output operations for creating a specific ADaM dataset according to the
#' provided specifications.
#'
#' @section File Requirements:
#' \itemize{
#'   \item UI data file must be a valid YAML file with ADaM specifications
#'   \item Trial metadata file must contain valid study configuration
#'   \item Trial path must be accessible for file operations
#' }
#'
#' @section Error Handling:
#' The function will stop execution if:
#' \itemize{
#'   \item UI data or trial metadata files cannot be read or are invalid
#'   \item Dependency validation fails (missing required columns)
#'   \item Trial configuration is malformed
#' }
#'
#' @examples
#' \dontrun{
#' # Generate ADaM programs with full dependency checking
#' result <- generate_adam_code(
#'   path_ui_data = "path/to/ui_specs.yml",
#'   path_trial_metadata = "path/to/trial_metadata.yml",
#'   path_trial = "path/to/trial_directory",
#'   check_cross_domain_adam_dependencies = TRUE
#' )
#'
#' # Access generated programs
#' adsl_program <- result$programs$ADSL
#' executable_programs <- result$executable_programs
#'
#' # Generate with custom standards library
#' result_custom <- generate_adam_code(
#'   path_ui_data = "ui_specs.yml",
#'   standards_lib = my_custom_standards,
#'   path_trial_metadata = "trial_config.yml",
#'   path_trial = "trial_data/",
#'   check_cross_domain_adam_dependencies = FALSE
#' )
#' }
#'
#' @import data.table
#' @export
generate_adam_code <- function(
  path_ui_data,
  standards_lib = NULL,
  path_trial_metadata,
  path_trial,
  check_cross_domain_adam_dependencies = TRUE,
  data_context = NULL
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
  actions_05_read <- add_read_data_actions(actions_04_org)
  actions_06_write <- add_write_data_actions(actions_05_read)
  actions_07_check <- add_check_executable_status(actions_06_write, ui_yml, data_context)
  # Create programs
  actions_08_code <- render_code(
    actions = actions_06_write,
    domain_keys = domain_keys,
    ui_data = ui_yml,
    path_trial = path_trial
  )
  actions_08_available_data <- render_code(
     actions = actions_07_check$actions,
     domain_keys = domain_keys,
     ui_data = ui_yml,
     path_trial = path_trial,
     available_data = actions_07_check$available_columns
  )
  return(
    list(
      programs = compile_into_programs(actions_08_code),
      program_sequence = actions_08_code,
      executable_programs = compile_into_programs(actions_08_available_data),
      executable_program_sequence = actions_07_check$actions,
      edges = edges,
      actions = actions_configuration$actions,
      rendered_components = actions_configuration$code_components_rendered
    )
  )
}
