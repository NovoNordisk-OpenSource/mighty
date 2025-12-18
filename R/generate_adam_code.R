#' Generate Complete Set of ADaM Programs
#'
#' This function orchestrates the complete workflow for generating ADaM (Analysis
#' Data Model) programs from ADaM specifications and trial metadata.
#' It processes specifications, validates dependencies, creates action sequences, and
#' renders executable R code for ADaM dataset creation.
#'
#' @param adam_specifications Character string. File path to the ADaM specifications
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
#' @return A named list containing the complete ADaM program generation results:
#'   \describe{
#'     \item{programs}{Named list of complete R programs, one per ADaM domain. Each element
#'       is a character string containing a fully executable R script. Programs include all
#'       dependency-ordered actions and are ready to execute independently.
#'       Names are prefixed with the required execution order.}
#'     \item{program_sequence}{*For debugging only*. Data.table containing the complete action sequence with fully
#'       rendered R code for all programs. Each row represents a single action with action
#'       metadata. Actions are ordered by dependency requirements within each domain. This
#'       provides a detailed view of the execution plan before compilation into complete programs.}
#'     \item{executable_programs}{Named list of programs that can be executed with the currently
#'       available data sources (as determined by `data_context`). Structure identical to `programs`
#'       but includes only code where all required input data is available. If `data_context`
#'       is NULL, this will match `programs`. Used to identify which ADaM derivations can be generated
#'       given the current data availability.}
#'     \item{executable_program_sequence}{*For debugging only*. Data.table containing the action sequence for executable
#'       programs only. Structure similar to `program_sequence` but filtered to include only actions
#'       from code that can be executed with available data. Provides visibility into which
#'       specific transformations will run when executing the available programs.}
#'     \item{edges}{*For debugging only*. Data.table defining the dependency graph between actions.
#'       Contains columns `parent_node` and `node_id`, representing directed edges where parent
#'       actions must execute before child actions. Edges are created from both column dependencies
#'       (when one action produces a column another action consumes) and row dependencies
#'       (explicit row-level operations). Includes synthetic edges connecting actions with no
#'       dependencies to domain initialization actions. Self-referential edges are removed.
#'       Used for debugging action execution order and dependency resolution.}
#'     \item{actions}{*For debugging only*. Data.table containing base action configurations
#'       before code rendering and program organization. Each row represents a single action with
#'       columns: `node_id` (unique action identifier), `domain` (ADaM domain name), `code_id`
#'       (reference to code component or NA), `type` (action type: col_copy, col_mutate, col_echo,
#'       col_compute, row_compute, init_domain, or filter_domain), `outputs` (list column of
#'       character vectors showing columns produced), `depend_cols` (nested data.table with
#'       column_name, domain, and domain_type showing column dependencies), `depend_rows` (list
#'       of node_ids this action depends on for row operations), and `parameters` (named list of
#'       user-provided parameters). Reflects the internal action data model before execution
#'       ordering. Used for debugging action setup and dependency validation.}
#'     \item{rendered_components}{*For debugging only*. Named list of code components that were successfully
#'       rendered using Mustache templates during the generation process. Each element contains
#'       the rendered R code for a specific component (e.g., derivation functions). Component
#'       names correspond to `code_id` values referenced in the action specifications. Used for
#'       inspecting how template parameters were resolved and for debugging component rendering.}
#'   }
#'
#' @details
#' The function executes the following workflow:
#' \enumerate{
#'   \item Reads and validates ADaM specifications and trial metadata
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
#'   \item ADaM specification file must be a valid YAML file with ADaM
#'     specifications following the schema defined in mighty.metadata
#'   \item Trial metadata file must contain valid study configuration
#'   \item Trial path must be accessible for file operations
#' }
#'
#' @section Error Handling:
#' The function will stop execution if:
#' \itemize{
#'   \item ADaM specifications or trial metadata files cannot be read or are
#'     invalid
#'   \item Dependency validation fails (missing required columns)
#'   \item Trial configuration is malformed
#' }
#'
#' @examples
#' \dontrun{
#' # Generate ADaM programs with full dependency checking
#' result <- generate_adam_code(
#'   adam_specifications = "path/to/ADaM_specs.yml",
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
#'   adam_specifications = "ADaM_specs.yml",
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
  adam_specifications,
  standards_lib = NULL,
  path_trial_metadata,
  path_trial,
  check_cross_domain_adam_dependencies = TRUE,
  data_context = NULL
) {
  # Read data from UI containing explicit user input
  ui_yml <- lapply(adam_specifications, read_mighty_metadata_adam_domain) |>
    unlist(recursive = FALSE)

  ui_init <- purrr::list_transpose(ui_yml)[["init"]]
  trial_metadata <- yaml::read_yaml(path_trial_metadata) |>
    assert_valid_trial_config()
  domain_keys <- collate_primary_keys(ui_yml, trial_metadata)

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
  actions_07_check <- add_check_executable_status(
    actions_06_write,
    ui_yml,
    data_context
  )
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
