#' Setup Actions for ADaM Code Generation
#'
#' Converts UI YAML specifications into the internal action model with
#' rendered code components and validated dependencies.
#'
#' @param ui_yml List. UI YAML specifications for ADaM domains, typically
#'   containing column metadata and domain configurations.
#' @param ui_init_t List. Transposed init metadata from UI YAML, containing
#'   filter dependencies and domain configurations per domain.
#' @param check_cross_domain_adam_dependencies Logical. Whether to validate
#'   cross-domain dependencies in ADaM specifications.
#' @param domain_keys Character vector. Domain key variables for dependency
#'   validation and metadata enrichment.
#'
#' @return Named list with two elements:
#'   \describe{
#'     \item{code_components_rendered}{Rendered code components ready for generation}
#'     \item{actions}{Data.table with processed action definitions including
#'       node_id, domain, dependencies, and validated metadata}
#'   }
#'
#' @details
#' Processing pipeline:
#' \enumerate{
#'   \item Converts UI YAML to tabular format via [convert_yml_to_data_table()]
#'   \item Extracts base actions and renders code components
#'   \item Consolidates metadata and adds domain keys
#'   \item Validates dependencies and cross-domain references
#' }
#'
#' The function transforms user specifications into standardized internal
#' format for downstream code generation functions.
#'
#' @noRd
setup_actions <- function(
  ui_yml,
  ui_init_t,
  check_cross_domain_adam_dependencies,
  domain_keys
) {
  checkmate::assert_list(ui_yml)

  ui_table <- convert_yml_to_data_table(ui_yml)
  ui_table$column <- NULL

  actions_base <- extract_actions(ui_table)

  # Render code components for each action given the specifications per action
  components_rendered <- render_components(actions_base)

  actions <- components_rendered |>
    get_component_metadata() |>
    consolidate_metadata(actions_base) |>
    add_keys_to_depend_cols(domain_keys, ui_init_t) |>
    assert_valid_depend_cols(
      ui_yml,
      domain_keys,
      check_cross_domain_adam_dependencies
    )

  return(list(
    code_components_rendered = components_rendered,
    actions = actions
  ))
}
