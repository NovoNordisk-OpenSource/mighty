#' Add Filter Domain Actions
#'
#' @description
#' Creates and adds filter_domain actions to the existing actions data table
#' based on UI initialization data and domain filtering requirements.
#'
#' @details
#' This function processes domains that require filtering by:
#' 1. Identifying domains with filter dependencies from UI initialization data
#' 2. Extracting external domain dependencies and validating their keys
#' 3. Creating comprehensive dependency lists including required join keys
#' 4. Generating filter_domain actions with appropriate inputs and outputs
#' 5. Handling both column copies and computed columns as filter outputs
#'
#' The function ensures that all external domain keys are available and creates
#' proper dependency chains for filtering operations across multiple domains.
#'
#' Throws error if external domains referenced in filter dependencies are not found
#' in the domain_keys lookup table.
#'
#' @param actions Data table containing existing action definitions with columns
#'   including node_id, type, domain, outputs, and depend_cols.
#' @param ui_init List containing UI initialization data with filter dependencies
#'   and domain filter specifications for each domain.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   respective key columns required for joining operations.
#'
#' @return
#' Data table combining the original actions with newly created filter_domain
#' actions, maintaining the same structure and column definitions.
#'
add_filter_domain_actions <- function(actions, ui_init, domain_keys) {
  # Filter dependencies
  filter_depends_cols <- purrr::list_transpose(ui_init)$filter_depend_cols

  # Determine which ADaM domains has filters
  domains_w_filter <- lapply(filter_depends_cols, function(x) any(!is.na(x))) |>
    unlist() |>
    which() |>
    names()

  # Extract external domains from filter dependencies
  filter_ext_domains <- lapply(
    filter_depends_cols[domains_w_filter],
    function(x) unique(gsub("\\..*$", "", x[grep("\\.", x)]))
  )

  # Extend filter dependencies with keys required for joining external dependencies
  filter_depends_cols_w_keys <- lapply(domains_w_filter, function(d) {
    # Check if external domains are defined in key look-up data. If not throw an error.
    unknown_ext_domains <- setdiff(
      toupper(filter_ext_domains[[d]]),
      names(domain_keys)
    )
    if (length(unknown_ext_domains) > 0) {
      stop(
        "Error: Unable to look up keys for the following domains: ",
        paste(unknown_ext_domains, collapse = ", "),
        ". Please check if the domains are valid and specified in trial metadata."
      )
    }

    # Get key columns for external domains
    keys <- domain_keys[toupper(filter_ext_domains[[d]])]

    # Expand keys to cover external and current ADaM domain
    filter_depends_keys <- lapply(filter_ext_domains[[d]], function(nm) {
      c(keys[[toupper(nm)]], paste0(nm, ".", keys[[toupper(nm)]]))
    }) |>
      unlist()

    # Collect keys and existing filter dependencies
    filter_depends_cols_i <- c(filter_depends_cols[[d]], filter_depends_keys) |>
      unique()

    # Add prefix for self domain
    is_self <- !grepl(".*\\..*", filter_depends_cols_i)
    filter_depends_cols_i[is_self] <- paste0(
      d,
      ".",
      filter_depends_cols_i[is_self]
    )

    v <- strsplit(filter_depends_cols_i, "\\.")
    dt <- data.table(
      column_name = sapply(v, `[`, 2),
      domain = sapply(v, `[`, 1)
    )
    dt[["domain_type"]] <- classify_data_domains(dt[["domain"]])
    return(dt)
  })
  names(filter_depends_cols_w_keys) <- domains_w_filter

  has_domain_filter <- lapply(
    purrr::list_transpose(ui_init)$filter_domain,
    function(x) {
      any(!is.na(unlist(x)))
    }
  )

  # Create filter_domain actions
  domain_filter_actions <- lapply(domains_w_filter, function(d) {
    # Create outputs for filter_domain

    # This has two sources:
    # 1) outputs from all col_copys that are consumed by init_domain
    #    and must be also to be outputted from filter_domain
    # 2) all outputs from filter dependency col_computes

    # 1) Get output of col_copy
    col_copy_outputs <- actions[
      actions$type == "col_copy" &
        actions$domain == d,
    ]$outputs |>
      unlist()

    # 2) Get all outputs of col_computes that are dependencies to the filter
    col_computes <- actions[
      actions$type == "col_compute",
      c("node_id", "outputs")
    ] |>
      tidyr::unnest("outputs") |>
      dplyr::filter(.data$outputs %in% filter_depends_cols[[d]])
    col_computes_all <-
      c(col_computes$node_id, get_all_parents(col_computes$node_id, actions))
    col_compute_outputs <- actions[
      actions$node_id %in% col_computes_all,
    ]$outputs |>
      unlist()

    # Collect the two sources of outputs
    filter_domain_outputs <- c(col_copy_outputs, col_compute_outputs)

    # Create depend_cols for filter_domain

    # This has two sources:
    # 1) Outputs inherited from domain_init, i.e. all col_copy + SRC_ (if domain filters are present)
    # 2) filter dependencies

    depend_cols_nm <-
      if (has_domain_filter[[d]]) {
        c(filter_domain_outputs, "SRC_")
      } else {
        filter_domain_outputs
      }
    inherited_depends_cols <-
      data.table(
        column_name = depend_cols_nm,
        domain = d,
        domain_type = classify_data_domains(d)
      )

    # Collect the two sources of depend_cols
    filter_domain_dep_cols <- unique(rbind(
      inherited_depends_cols,
      filter_depends_cols_w_keys[[d]]
    ))

    # Consolidate domain_filter action for domain d
    data.table(
      node_id = paste0(d, "-filter_domain"),
      code_id = "_filter_domain.mustache",
      type = "filter_domain",
      depend_cols = list(filter_domain_dep_cols),
      outputs = list(filter_domain_outputs),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = d
    )
  }) |>
    rbindlist()

  # Add domain_filter actions to existing actions
  return(rbind(actions, domain_filter_actions))
}
