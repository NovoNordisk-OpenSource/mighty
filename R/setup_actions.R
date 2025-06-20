#' Setup actions for ADaM Code Generation
#'
#' @description Prepares the initial action components by processing UI YAML
#' data and creating a consolidated environment for code components.
#'
#' @details This function performs the initial setup for ADaM code generation
#' by: 1. Converting UI YAML specifications to a data.table format 2. Creating a
#' consolidated environment containing code components from packages and source
#' files 3. Parsing code component metadata and merging it with UI data 4.
#' Adding unique node identifiers and validating outputs
#'
#' The function serves as a preprocessing step that transforms user
#' specifications and code components into a standardized internal format that
#' can be used by downstream functions.
#'
#' @param ui_yml List containing UI YAML specifications for ADaM domains,
#'   typically read from YAML files containing column metadata and domain
#'   configurations
#' @param code_component_source_pkgs Optional character vector of package names
#'   containing code components to be loaded into the consolidated environment
#' @param code_component_source_files Optional character vector of file paths to
#'   R source files containing code components
#'
#' @return A list containing two elements:
#'   \item{code_component_env}{Environment containing consolidated code components
#'     from specified packages and source files}
#'   \item{nodes}{Data.table containing processed node definitions with columns
#'     for node_id, domain, type, dependencies, outputs, and other metadata}
#'
#' @seealso \code{\link{convert_yml_to_data_table}} for YAML to data.table
#' conversion \code{\link{create_consolidated_env}} for environment creation
#' \code{\link{parse_code_components_metadata}} for metadata parsing
#' \code{\link{update_ui_data}} for UI data enrichment
#'
#' @examples
#' \dontrun{
#' # Prepare pipeline with package-based code components
#' ui_data <- read_adam_specs("path/to/yaml/files")
#' pipeline_prep <- setup_actions(
#'   ui_yml = ui_data,
#'   code_component_source_pkgs = c("mighty.standards", "custom.package"),
#'   code_component_source_files = NULL
#' )
#'
#' # Access the prepared components
#' code_env <- pipeline_prep$code_component_env
#' nodes <- pipeline_prep$nodes
#' }
setup_actions <- function(ui_yml, code_component_source_pkgs, code_component_source_files){
  checkmate::assert_list(ui_yml)

  ui_table <- convert_yml_to_data_table(ui_yml)

  # Create a consolidated environment for code components
  unique_code_ids <- ui_table[!is.na(code_id) & !duplicated(code_id), code_id]
  code_component_env <- create_consolidated_env(
    packages = code_component_source_pkgs,
    source_files = code_component_source_files,
    code_ids = unique_code_ids
  )

  # Combine UI data and standard components
  actions <- parse_code_components_metadata(pkgs = code_component_source_pkgs,
                                 source_files = code_component_source_files,
                                 function_names = unique_code_ids) |>
    update_ui_data(ui_table) |>
    add_node_id_fast() |>
    assert_valid_outputs() |>
    assign_predecessor_action_types()

  return(list(code_component_env=code_component_env,
              actions = actions))
}
