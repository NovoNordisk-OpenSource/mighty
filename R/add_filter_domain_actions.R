#' Add Filter Domain Actions
#'
#' @description
#' Creates filter_domain actions that apply row-level population filters defined
#' in the YAML spec's `population` section (both `base` and `global` filters).
#'
#' Filter conditions can reference columns from external domains (e.g.,
#' `ADSL.SAFFL` when filtering ADLB). This function ensures the required join
#' keys are included as dependencies so external columns can be looked up at
#' runtime.
#'
#' @details
#' This is separate from init_domain because filtering may require joins to
#' external datasets that aren't available during initialization. The
#' init_domain action row-binds the base datasets, while filter_domain joins
#' any external datasets needed for filter conditions, then applies the filters.
#'
#'
#' For each domain with filter dependencies, this function:
#' 1. Parses external domain references (e.g., "ADSL.SAFFL" -> domain "ADSL")
#' 2. Looks up join keys for external domains from trial metadata
#' 3. Builds a dependency list combining filter columns and join keys
#' 4. Creates a filter_domain action with appropriate inputs/outputs
#'
#' Throws an error if external domains are not found in the domain_keys lookup.
#'
#' @param actions Data table containing existing action definitions with columns
#'   including node_id, type, domain, outputs, and depend_cols.
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @param filter_domain Named list of filter domain specifications per domain.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   respective key columns required for joining operations.
#'
#' @return
#' Data table combining the original actions with newly created filter_domain
#' actions, maintaining the same structure and column definitions.
#' @noRd
add_filter_domain_actions <- function(
  actions,
  filter_depend_cols,
  filter_domain,
  domain_keys
) {
  # Input validation

  checkmate::assert_data_table(actions)
  checkmate::assert_names(
    names(actions),
    must.include = c("node_id", "type", "domain", "outputs", "depend_cols")
  )
  checkmate::assert_list(filter_depend_cols, names = "named")
  checkmate::assert_list(filter_domain, names = "named")
  checkmate::assert_set_equal(names(filter_depend_cols), names(filter_domain))
  checkmate::assert_list(domain_keys, names = "named")

  domains_w_filter <- get_domains_with_filters(filter_depend_cols)

  # Extract external domains from filter dependencies
  filter_ext_domains <- parse_external_domain_refs(
    filter_depend_cols,
    domains_w_filter
  )

  # Extend filter dependencies with keys required for joining external dependencies
  filter_depend_cols_w_keys <- lapply(domains_w_filter, function(domain) {
    build_filter_deps_with_keys(
      domain,
      filter_depend_cols[[domain]],
      filter_ext_domains[[domain]],
      domain_keys
    )
  }) |>
    setNames(domains_w_filter)

  has_domain_filter <- has_domain_level_filter(filter_domain)

  # Create filter_domain actions
  domain_filter_actions <- lapply(domains_w_filter, function(domain) {
    create_filter_domain_action(
      target_domain = domain,
      actions = actions,
      domain_filter_depends = filter_depend_cols[[domain]],
      filter_deps_w_keys = filter_depend_cols_w_keys[[domain]],
      has_domain_filter = has_domain_filter[[domain]]
    )
  }) |>
    rbindlist()

  # Add domain_filter actions to existing actions
  rbind(actions, domain_filter_actions)
}

#' Find domains that have filter dependencies
#'
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @return Character vector of domain names that have filters.
#' @noRd
get_domains_with_filters <- function(filter_depend_cols) {
  has_filter <- vapply(
    filter_depend_cols,
    \(x) any(!is.na(x)),
    logical(1)
  )
  names(which(has_filter))
}

#' Parse external domain references from filter dependencies
#'
#' Extracts domain names from "domain.column" format strings.
#'
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @param domains_w_filter Character vector of domain names that have filters.
#' @return Named list of external domain names per ADaM domain.
#' @noRd
parse_external_domain_refs <- function(filter_depend_cols, domains_w_filter) {
  filter_dependencies <- filter_depend_cols[domains_w_filter]
  lapply(filter_dependencies, function(cols) {
    domain_prefixed <- cols[has_domain_prefix(cols)]
    extract_domain_prefix(domain_prefixed) |> unique()
  })
}

#' Get join keys for external domains referenced in filters
#'
#' Returns both unprefixed and prefixed key columns for each external domain.
#' Both are needed because joining requires matching keys from both sides:
#' unprefixed keys for the target domain, prefixed keys for the external domain.
#'
#' @param ext_domains Character vector of external domain names referenced in
#'   filter conditions (e.g., "ADSL" from "ADSL.SAFFL").
#' @param domain_keys Named list mapping domain names to their key columns.
#' @param target_domain Character string of the ADaM domain being filtered.
#'   Used in error messages.
#' @return Character vector of key columns (both prefixed and unprefixed),
#'   or NULL if ext_domains is empty.
#' @noRd
get_filter_join_keys_external_domains <- function(
  ext_domains,
  domain_keys,
  target_domain
) {
  if (length(ext_domains) == 0) {
    return(NULL)
  }

  unknown_domains <- setdiff(toupper(ext_domains), names(domain_keys))
  if (length(unknown_domains) > 0) {
    n <- length(unknown_domains)
    domain_list <- format_list(unknown_domains, format_domain)
    filter_msg <- paste0(
      "Filter for ",
      format_domain(target_domain),
      " references ",
      n,
      cli::format_inline("{cli::qty(n)} unknown domain{?s}: "),
      domain_list
    )
    info_msg <- cli::format_inline(
      "{cli::qty(n)}{?This domain/These domains} must have join keys defined to be used in filters"
    )

    throw_validation_error(
      category = "Unknown domains in filter",
      details = c(
        "x" = filter_msg,
        "i" = info_msg
      ),
      suggestions = c(
        "Add key definitions to {.file _mighty.yml} {.field /external_data} for the referenced domains",
        "Verify the domain names are spelled correctly",
        "Ensure all domains used in filters are defined in your trial metadata"
      )
    )
  }

  keys <- domain_keys[toupper(ext_domains)]
  lapply(ext_domains, function(nm) {
    key_cols <- keys[[toupper(nm)]]
    c(key_cols, paste0(nm, ".", key_cols))
  }) |>
    unlist()
}

#' Build filter dependencies with join keys for one domain
#'
#' Combines filter dependency columns with join keys from external domains,
#' then converts to a data.table with domain prefixes.
#'
#' @param domain Character string of the target ADaM domain name.
#' @param domain_filter_deps Character vector of filter dependency columns.
#' @param ext_domains Character vector of external domain names referenced in
#'   filter conditions.
#' @param domain_keys Named list mapping domain names to their key columns.
#' @return data.table with columns: column_name, domain, domain_type.
#' @noRd
build_filter_deps_with_keys <- function(
  domain,
  domain_filter_deps,
  ext_domains,
  domain_keys
) {
  keys_external_domains <- get_filter_join_keys_external_domains(
    ext_domains,
    domain_keys,
    domain
  )

  # Collect keys and existing filter dependencies
  all_filter_deps <- c(
    domain_filter_deps,
    keys_external_domains
  ) |>
    unique()

  # Add prefix for self domain
  is_self <- !grepl("\\.", all_filter_deps)
  all_filter_deps[is_self] <- paste0(
    domain,
    ".",
    all_filter_deps[is_self]
  )

  dt <- all_filter_deps |>
    tstrsplit("\\.") |>
    as.data.table()

  setnames(dt, c("domain", "column_name"))
  dt[["domain_type"]] <- classify_data_domains(dt[["domain"]])
  dt
}

#' Get col_compute outputs needed for filtering
#'
#' Finds col_compute actions whose outputs are filter dependencies, then
#' collects outputs from those actions and all their parent col_computes.
#' Parent outputs are included because the filter_domain action needs the
#' full dependency chain available.
#'
#' @param actions Data table containing existing action definitions.
#' @param target_domain Character string of the domain name.
#' @param filter_depends Character vector of filter dependency columns.
#' @return Character vector of outputs from matching col_computes and their parents.
#' @noRd
get_col_compute_filter_outputs <- function(
  actions,
  target_domain,
  filter_depends
) {
  col_computes <- actions[type == "col_compute" & domain == target_domain]
  if (nrow(col_computes) == 0) {
    return(character(0))
  }

  outputs_by_node <- col_computes[, .(output = unlist(outputs)), by = node_id]
  matching_nodes <- outputs_by_node[output %in% filter_depends, unique(node_id)]
  if (length(matching_nodes) == 0) {
    return(character(0))
  }

  all_nodes <- c(matching_nodes, get_all_parents(matching_nodes, actions))
  actions[node_id %in% all_nodes, unlist(outputs)]
}

#' Create a single filter_domain action for one domain
#'
#' Assembles a filter_domain action that declares:
#' - outputs: columns passed through from col_copy and col_compute actions
#' - depend_cols: columns needed from init_domain plus external filter dependencies
#'
#' @param target_domain Character string of the domain name.
#' @param actions Data table containing existing action definitions.
#' @param domain_filter_depends Character vector of filter dependency columns for this domain.
#' @param filter_deps_w_keys data.table of filter dependencies with join keys.
#' @param has_domain_filter Logical indicating if domain has base-level filters
#'   (determines whether SRC_ column is included in dependencies).
#' @return Single-row data.table representing the filter_domain action.
#' @noRd
create_filter_domain_action <- function(
  target_domain,
  actions,
  domain_filter_depends,
  filter_deps_w_keys,
  has_domain_filter
) {
  # Create outputs for filter_domain from two sources:
  # 1) outputs from all col_copys that are consumed by init_domain
  # 2) all outputs from filter dependency col_computes
  col_copy_outputs <- actions[
    type == "col_copy" & domain == target_domain,
    unlist(outputs)
  ]
  col_compute_outputs <- get_col_compute_filter_outputs(
    actions,
    target_domain,
    domain_filter_depends
  )
  filter_domain_outputs <- c(col_copy_outputs, col_compute_outputs)

  # Create depend_cols for filter_domain

  # This has two sources:
  # 1) Outputs inherited from domain_init, i.e. all col_copy + SRC_ (if domain filters are present)
  # 2) filter dependencies

  depend_cols_nm <-
    if (has_domain_filter) {
      c(filter_domain_outputs, "SRC_")
    } else {
      filter_domain_outputs
    }
  inherited_depends_cols <- data.table(
    column_name = depend_cols_nm,
    domain = target_domain,
    domain_type = classify_data_domains(target_domain)
  )

  # Collect the two sources of depend_cols
  filter_domain_dep_cols <- unique(rbind(
    inherited_depends_cols,
    filter_deps_w_keys
  ))

  # Consolidate domain_filter action for domain
  data.table(
    node_id = paste0(target_domain, "-filter_domain"),
    code_id = "_filter_domain.mustache",
    type = "filter_domain",
    depend_cols = list(filter_domain_dep_cols),
    outputs = list(filter_domain_outputs),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = target_domain
  )
}
