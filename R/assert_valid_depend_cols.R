#' Validate Dependency Columns
#'
#' This function validates that all specified dependency columns for each action
#' in a dataset are present in the corresponding outputs. It performs comprehensive
#' dependency checking both within and across ADaM domains, including filter
#' dependencies and action dependencies.
#'
#' @param actions A data.frame containing action metadata with required columns:
#'   \describe{
#'     \item{type}{Character. Action type (e.g., "col_*" for column actions)}
#'     \item{domain}{Character. ADaM domain identifier}
#'     \item{depend_cols}{List. Column dependencies for each action}
#'     \item{outputs}{List. Expected output columns for each action}
#'   }
#' @param ui_yml A list containing UI configuration with initialization parameters
#'   used for filtering and extracting dependency columns across domains.
#' @param domain_keys Character vector of domain key identifiers used to extract
#'   filter dependencies and establish cross-domain relationships.
#' @param check_cross_domain_adam_dependencies Logical. If `TRUE`, performs
#'   cross-domain dependency validation. If `FALSE`, only validates dependencies
#'   within each individual ADaM domain.
#'
#' @return Invisibly returns the input `actions` data.frame unchanged if all
#'   dependency validations pass.
#'
#' @details
#' The function performs the following validation steps:
#' \enumerate{
#'   \item Extracts ADaM column dependencies from UI filters, including implied
#'         join keys for external dependencies
#'   \item Splits actions by domain for domain-specific processing
#'   \item Combines dependencies from two sources: filters on the domain and
#'         actions within the domain
#'   \item Extracts outputs from column actions (identified by "col" prefix)
#'   \item Validates dependencies either within domains only or across all domains
#'         based on the `check_cross_domain_adam_dependencies` parameter
#' }
#'
#' @section Error Handling:
#' The function stops execution with an informative error message if any
#' dependency columns are missing from the outputs for any domain. Error
#' messages indicate which columns are missing and which actions require them.
#'
#' @examples
#' \dontrun{
#' # Example actions data.frame
#' actions <- data.frame(
#'   type = c("col_derive", "col_merge"),
#'   domain = c("ADSL", "ADAE"),
#'   depend_cols = list(c("USUBJID"), c("USUBJID", "AEDECOD")),
#'   outputs = list(c("USUBJID", "AGE"), c("USUBJID", "AEDECOD", "AESEV"))
#' )
#'
#' # Validate dependencies within domains only
#' assert_valid_depend_cols(actions, ui_yml, domain_keys,
#'   check_cross_domain_adam_dependencies = FALSE
#' )
#' }
#'
#' @seealso
#' [get_filter_adam_dependencies()] for extracting filter dependencies,
#' [get_all_adam_dependencies()] for combining dependency sources,
#' [check_adam_dependencies_cross_domain()] for cross-domain validation,
#' [check_adam_dependencies_within_domain()] for within-domain validation
#'
#' @noRd
assert_valid_depend_cols <- function(
  actions,
  ui_yml,
  domain_keys,
  check_cross_domain_adam_dependencies
) {
  # Extract ADaM column dependencies from filters incl. implied join keys for external dependencies
  filter_dep_by_domain <- get_filter_adam_dependencies(ui_yml, domain_keys)

  # Split actions by domain
  actions_by_domain <- split(
    actions[, c("domain", "depend_cols", "outputs", "code_id")],
    by = "domain"
  )

  # Extract column dependencies per domain from two sources:
  #   - filters on the domain
  #   - actions within the domain
  adam_dep_by_domain <- lapply(
    actions_by_domain,
    get_all_adam_dependencies,
    filter_dep_by_domain
  )

  # Extract outputs from column actions prefixed with ADaM domain
  is_column_action <- substr(actions$type, 1, 3) == "col"
  outputs <- get_outputs(actions[is_column_action])

  # col_rename source columns are provided by init_domain (from SDTM), so
  # they count as available outputs for dependency validation
  col_rename_actions <- actions[actions$type == "col_rename"]
  if (nrow(col_rename_actions) > 0) {
    rename_source_outputs <- purrr::map2(
      col_rename_actions$domain,
      col_rename_actions$depend_cols,
      function(domain, dc) paste0(domain, ".", dc$column_name)
    ) |> unlist()
    outputs <- c(outputs, rename_source_outputs) |> unique()
  }

  if (check_cross_domain_adam_dependencies) {
    check_adam_dependencies_cross_domain(
      actions,
      adam_dep_by_domain,
      outputs,
      filter_dep_by_domain
    )
  } else {
    # Only check that there are no missing internal parents per ADaM domain
    for (nm in names(actions_by_domain)) {
      check_adam_dependencies_within_domain(
        nm,
        adam_dep_by_domain,
        outputs,
        actions_by_domain,
        filter_dep_by_domain
      )
    }
  }

  return(invisible(actions))
}

#' Get Filter ADaM Dependencies
#'
#' This function extracts filter dependency columns from the provided UI YAML
#' and enriches them with additional columns for joining any external filter dependencies.
#' It identifies and returns the dependencies that are specific to ADaM data domains.
#'
#' @param ui_yml A list containing the UI YAML structure which includes an
#' initialization section where filter dependencies are defined.
#' @param domain_keys A named list containing keys associated with each domain
#' for joining external filter dependencies.
#'
#' @return A named list of filter dependency columns enriched with keys for
#' external dependencies, specifically for the ADaM data domain.
#' @noRd
get_filter_adam_dependencies <- function(ui_yml, domain_keys) {
  # Transpose the initialization section from the UI YAML to extract filter dependency columns
  ui_init <- purrr::list_transpose(ui_yml)[["init"]]
  filter_depends_cols <- purrr::list_transpose(ui_init)$filter_depend_cols

  # Enrich filter dependency columns with columns for joining any external filter dependencies
  filter_depends_cols_w_keys <- lapply(
    names(filter_depends_cols),
    function(nm) {
      # Extract column dependencies for the domain
      dep_cols_i <- filter_depends_cols[[nm]]

      if (any(!is.na(dep_cols_i))) {
        # Identify external domains and associated dependency columns for joining
        is_external <- has_domain_prefix(dep_cols_i)
        external_domains <- dep_cols_i[is_external] |>
          extract_domain_prefix() |>
          unique()

        keys_i <- lapply(external_domains, function(j) {
          c(
            paste0(nm, ".", domain_keys[[toupper(j)]]),
            paste0(j, ".", domain_keys[[toupper(j)]])
          )
        }) |>
          unlist()

        # Update internal domain with prefix
        dep_cols_i[!is_external] <- paste0(nm, ".", dep_cols_i[!is_external])

        # Collect filter column dependencies and keys
        dep_cols_i_updated <- unique(c(dep_cols_i, keys_i))
        is_adam <- classify_data_domains(gsub(
          "\\..*",
          "",
          dep_cols_i_updated
        )) ==
          "adam"

        # Return ADaM dependencies
        return(dep_cols_i_updated[is_adam])
      } else {
        return(dep_cols_i)
      }
    }
  )

  names(filter_depends_cols_w_keys) <- names(filter_depends_cols)
  return(filter_depends_cols_w_keys)
}

#' Get All ADaM Dependencies for a Domain
#'
#' @description
#' Identifies all ADaM column dependencies for a specific domain from both
#' filter conditions and action dependencies.
#'
#' @details
#' Combines two sources of dependencies:
#' 1. ADSL column dependencies used in domain filters
#' 2. Column dependencies from actions within the domain
#'
#' The result is a unique set of all column dependencies needed for the domain.
#'
#' @param x Data frame or list containing domain information with 'domain' and
#'   'depend_cols' elements.
#' @param filter_dep List mapping domain names to their ADSL filter dependencies.
#'
#' @return
#' Character vector of unique column dependencies for the domain, with NAs removed.
#'
#' @noRd
get_all_adam_dependencies <- function(x, filter_dep) {
  # Extract column dependencies in filter
  filter_depend_cols <- filter_dep[[x$domain[[1]]]]

  # Find column dependencies in actions
  depend_cols <- lapply(x$depend_cols, get_adam_dependencies_from_actions) |>
    unlist()

  # Combine the two sources and return set of unique column dependencies
  depend_cols_combined <- c(filter_depend_cols, depend_cols) |>
    unique()

  # Remove NAs
  depend_cols_combined[!is.na(depend_cols_combined)]
}

#' Get ADaM dependencies from action
#' @noRd
get_adam_dependencies_from_actions <- function(depend_col) {
  if (all(!is.na(depend_col))) {
    # Filter for dependencies from ADaM domains
    adam_deps <- depend_col[domain_type == "adam", ]
    if (nrow(adam_deps) == 0) {
      return(character(0))
    }
    return(qualify_column_refs(adam_deps))
  }
}

#' Extract outputs across all actions
#' @description
#' Creates fully specified column identifiers by prefixing output columns with their domain names,

#' @param x data.table containing columns 'domain', 'outputs', and 'type'
#'
#' @returns Character vector of domain-prefixed column names in the format "domain.column_name"
#' @noRd
get_outputs <- function(x) {
  # Create fully qualified output names by prefixing with domain
  outputs <- purrr::map2(x$domain, x$outputs, function(domain, output) {
    paste0(domain, ".", unlist(output))
  }) |>
    unlist()

  return(outputs)
}

#' Check for External ADaM Dependencies
#'
#' @description
#' Validates that all required ADaM column dependencies across domains are
#' available in the outputs, stopping with a detailed error if any are missing.
#'
#' @param actions Data frame containing action dependency information.
#' @param adam_dep_by_domain List mapping domain names to their ADaM dependencies.
#' @param outputs Character vector of available output columns.
#' @param filter_dep_by_domain List mapping domain names to their filter dependencies.
#'
#' @return
#' Invisible NULL if all dependencies are satisfied. Stops execution with a
#' detailed error message listing missing columns and affected actions/filters
#' if dependencies are not met.
#'
#' @details
#' The function identifies missing ADaM dependencies by comparing required
#' columns against available outputs. It collects information on which actions
#' and domain filters are affected by the missing dependencies and generates
#' an informative error message that lists the missing columns and the
#' components that require them.
#'
#' @noRd
check_adam_dependencies_cross_domain <- function(
  actions,
  adam_dep_by_domain,
  outputs,
  filter_dep_by_domain
) {
  # Identify missing ADaM column dependencies across domains
  all_adam_dep <- adam_dep_by_domain |>
    unlist() |>
    as.character()
  missing_adam_deps <- setdiff(all_adam_dep, outputs)

  # Return early if there are no missing dependencies
  if (length(missing_adam_deps) == 0) {
    return(invisible(NULL))
  }

  # Collect affected actions with component names
  idx <- vapply(
    actions$depend_cols,
    function(x) {
      any(qualify_column_refs(x) %in% missing_adam_deps)
    },
    logical(1)
  )

  affected_actions <- vapply(
    which(idx),
    function(i) {
      action <- actions[i, ]
      format_action_with_component(
        action$domain,
        unlist(action$output),
        action$code_id
      )
    },
    character(1)
  )

  # Collect affected filters
  domains <- unique(actions$domain)
  filter_affected <- vapply(
    domains,
    function(nm) {
      any(filter_dep_by_domain[[nm]] %in% missing_adam_deps)
    },
    logical(1)
  )
  affected_filters <- vapply(
    domains[filter_affected],
    function(nm) {
      paste0(format_domain(nm), " filter")
    },
    character(1)
  )

  # Combine affected components
  affected_components <- c(affected_actions, affected_filters)

  # Throw error with formatted missing dependencies
  throw_missing_dependencies_error(
    missing_deps = sort(missing_adam_deps),
    affected_components = affected_components
  )
}

#' Check for Missing Internal Parent Columns in ADaM Domain
#'
#' @description
#' Validates that all required parent columns within a specific ADaM domain
#' are available, returning an error message if any are missing.
#'
#' @param domain Character string representing the domain name to check.
#' @param adam_dep_by_domain List mapping domain names to their ADaM dependencies.
#' @param outputs Character vector of available output columns.
#' @param actions_by_domain List of data frames split by domain containing
#'   action dependency information including code_id.
#' @param filter_dep_by_domain List mapping domain names to their filter dependencies.
#'
#' @return
#' Invisible NULL if all dependencies are satisfied. Stops execution with a
#' detailed error message listing missing columns and affected actions/filters
#' within the domain if dependencies are not met.
#'
#' @details
#' The function focuses on internal dependencies within a single domain,
#' identifying missing parent columns that are required by actions or filters
#' in the same domain. It generates an informative error message showing which
#' columns are needed and which components or domain filters require them.
#'
#' @noRd
check_adam_dependencies_within_domain <- function(
  domain_name,
  adam_dep_by_domain,
  outputs,
  actions_by_domain,
  filter_dep_by_domain
) {
  # Safety check: ensure domain exists in adam_dep_by_domain
  if (is.null(adam_dep_by_domain[[domain_name]])) {
    return(invisible(NULL))
  }

  # Identify missing ADaM column dependencies for the specific domain
  missing_adam_deps <- setdiff(adam_dep_by_domain[[domain_name]], outputs)

  # Return early if there are no missing dependencies
  if (length(missing_adam_deps) == 0) {
    return(invisible(NULL))
  }

  # Extract missing column dependencies that belong to the specific domain
  missing_adam_deps_domain <- lapply(missing_adam_deps, function(x) {
    str_split <- strsplit(x, "\\.")[[1]]
    if (
      toupper(str_split[1]) == toupper(domain_name) & length(str_split) == 2
    ) {
      str_split[2]
    }
  }) |>
    unlist()

  # Return early if there are no missing dependencies within the domain
  if (length(missing_adam_deps_domain) == 0) {
    return(invisible(NULL))
  }

  # Find which actions are affected by the missing columns
  idx <- vapply(
    actions_by_domain[[domain_name]]$depend_cols,
    function(y) {
      any(
        y$column_name %in%
          missing_adam_deps_domain &
          toupper(y$domain) == toupper(domain_name)
      )
    },
    logical(1)
  )

  # Collect affected actions with component names
  domain_actions <- actions_by_domain[[domain_name]][idx, ]
  affected_actions <- vapply(
    seq_len(nrow(domain_actions)),
    function(i) {
      format_action_with_component(
        domain_name,
        unlist(domain_actions$outputs[[i]]),
        domain_actions$code_id[[i]]
      )
    },
    character(1)
  )

  # Collect affected filters
  has_missing_filter_dep <- any(
    filter_dep_by_domain[[domain_name]] %in%
      paste0(domain_name, ".", missing_adam_deps_domain)
  )
  affected_filters <- if (has_missing_filter_dep) {
    paste0(format_domain(domain_name), " filter")
  } else {
    character()
  }

  # Combine affected components
  affected_components <- c(affected_actions, affected_filters)

  # Throw error with formatted missing dependencies
  throw_missing_dependencies_error(
    missing_deps = paste0(domain_name, ".", sort(missing_adam_deps_domain)),
    affected_components = affected_components
  )
}

#' Format action outputs with component name
#' @noRd
format_action_with_component <- function(domain, outputs, code_id) {
  outputs_formatted <- format_list(
    paste0(domain, ".", outputs),
    format_qualified_column
  )

  component_str <- if (!is.na(code_id)) {
    paste0("via {.file ", basename(code_id), "}")
  } else {
    ""
  }

  paste(outputs_formatted, component_str)
}

#' Throw missing dependencies error
#' @noRd
throw_missing_dependencies_error <- function(
  missing_deps,
  affected_components
) {
  throw_validation_error(
    category = "Missing dependencies",
    details = c(
      "i" = paste0(
        "Missing columns: ",
        format_list(missing_deps, format_qualified_column)
      ),
      "i" = paste0(
        "Required by: ",
        format_list(unique(affected_components), identity)
      )
    ),
    suggestions = c(
      "Add missing columns to their respective domain specifications",
      "Remove dependencies on these columns if they're not needed",
      "Check for typos and casing in column or domain names",
      "Ensure all required domains are defined in your specifications"
    )
  )
}
