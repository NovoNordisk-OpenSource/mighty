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
#' @param check_cross_domain_adam_dependencies
#' @param domain_keys
#'
#' @return A list containing two elements:
#'   \item{nodes}{Data.table containing processed node definitions with columns
#'     for node_id, domain, type, dependencies, outputs, and other metadata}
#'
#' @seealso \code{\link{convert_yml_to_data_table}} for YAML to data.table
#' conversion
#' \code{\link{update_ui_data}} for UI data enrichment
#'
#' @examples

setup_actions <- function(
  ui_yml,
  standards_lib,
  check_cross_domain_adam_dependencies,
  domain_keys
) {

  checkmate::assert_list(ui_yml)

  # Extract init metadata
  ui_init <- purrr::list_transpose(ui_yml)[["init"]]

  # Get UI data in tabular format
  ui_table <- convert_yml_to_data_table(ui_yml)
  ui_table$column <- NULL

  actions_base <- extract_actions(ui_table)

  # Render code components for each action given the specifications per action
  components_rendered <- render_components(actions_base)

  actions <- components_rendered |>
    get_component_metadata() |>
    consolidate_metadata(actions_base) |>
    add_keys_to_depend_cols(domain_keys, ui_init) |>
    assert_valid_outputs() |>
    assert_valid_depend_cols(ui_yml, domain_keys, check_cross_domain_adam_dependencies)

  return(list(
    code_components_rendered = components_rendered,
    actions = actions
  ))

}
