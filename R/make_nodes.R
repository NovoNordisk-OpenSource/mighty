#' Process ADaM actions for topology generation
#'
#' @description Processes and enriches node definitions to create a complete
#' computational graph for ADaM dataset generation, including dependency
#' resolution and validation.
#' @param nodes Data.table containing initial node definitions with columns for
#'   node_id, domain, type, depend_cols, outputs, and other metadata
#' @param domain_keys Named list mapping domain names to their respective
#'   primary key columns (e.g., list(ADSL = "USUBJID", DM = c("STUDYID",
#'   "USUBJID")))
#' @param ui_init List containing UI initialization data with domain-specific
#'   configurations including core domains, filters, and dependencies
#' @param check_cross_domain_adam_dependencies Logical flag indicating whether
#'   to validate dependencies across different ADaM domains (TRUE) or only
#'   within each domain (FALSE). When TRUE, ensures all cross-domain references
#'   can be resolved.
#' @return Data.table containing processed and validated nodes with enriched
#'   dependency information, proper action types, and resolved domain
#'   references. The returned nodes are ready for use in computational graph
#'   construction and program generation.
processing_actions <- function(actions,
                       domain_keys,
                       ui_init,
                       check_cross_domain_adam_dependencies) {
  # Enrich depend_cols.
  # - For external col_echo actions: include foreign keys
  # - For col_compute actions that input a core column and return the same
  #   column in the ADaM domain: Add output columns from all other actions that
  #   have the same core column as input
  actions |>
    update_depend_cols(domain_keys, ui_init) |>
    # Create an initialize action per domain that absorbs col_copy action
    create_preprocess_domain_nodes() |>
    # Replace "core" with relevant domains
    replace_core_with_named_domain(ui_init) |>
    assert_valid_adam_dependencies(ui_init, domain_keys, check_cross_domain_adam_dependencies)
}
