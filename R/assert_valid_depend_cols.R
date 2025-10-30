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
#'                         check_cross_domain_adam_dependencies = FALSE)
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
    actions[, c("domain", "depend_cols", "outputs")],
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

  if (check_cross_domain_adam_dependencies) {
    check_adam_dependencies_cross_domain(
      actions,
      adam_dep_by_domain,
      outputs,
      filter_dep_by_domain
    )
  } else {
    # Only check that the are no missing internal parents per ADaM domain
    error_msg <- c()
    for (nm in names(actions_by_domain)) {
      error_msg <- c(
        error_msg,
        check_adam_dependencies_within_domain(
          nm,
          adam_dep_by_domain,
          outputs,
          actions_by_domain,
          filter_dep_by_domain
        )
      )
    }

    # If there are any missing dependencies, stop execution
    if (length(error_msg) > 0) {
      stop(error_msg)
    }
  }

  return(invisible(actions))
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
        is_external <- grepl("\\.", dep_cols_i)
        external_domains <- unique(gsub("\\..*", "", dep_cols_i[is_external]))
        external_domain_types <- classify_data_domains(external_domains)

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
  depend_cols <- purrr::map2(
    x$domain,
    x$depend_cols,
    get_adam_dependencies_from_actions
  ) |>
    unlist()

  # Combine the two sources and return set of unique column dependencies
  depend_cols_combined <- c(filter_depend_cols, depend_cols) |>
    unique()

  # Remove NAs
  depend_cols_combined[!is.na(depend_cols_combined)]
}

#' Get Column Dependencies from Actions
#' @description Extracts column dependencies from actions within a domain,
#' focusing on ADaM dependencies.
#'
#' @details Analyzes the domain and dependency columns to identify
#' dependencies that come from ADaM domains. It checks for columns that have
#' names starting with "AD" and constructs fully qualified column names
#' (domain.column_name).
#'
#' @param domain_table Character string representing the domain table name
#' @param depend_col Data frame containing dependency information with columns
#'   'domain' and 'column_name'
#'
#' @returns A character vector of fully qualified column names
#'   (domain.column_name) that represent dependencies from ADaM domains
#' @noRd
get_adam_dependencies_from_actions <- function(domain_table, depend_col) {
  if (all(!is.na(depend_col))) {
    # Filter for dependencies from ADaM domains
    adam_deps <- depend_col[domain_type == "adam", ]
    if (nrow(adam_deps) == 0) {
      return(character(0))
    }
    return(adam_deps[, paste0(domain, ".", column_name)])
  }
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
#' columns against available outputs. When dependencies are missing, it
#' generates an informative error showing which columns are needed and
#' which specific actions or domain filters require them.
#'
#' @noRd
check_adam_dependencies_cross_domain <- function(
  actions,
  adam_dep_by_domain,
  outputs,
  filter_dep_by_domain
) {
  # Identify missing ADaM column dependencies across domains
  all_adam_dep <- adam_dep_by_domain |> unlist() |> as.character()
  missing_adam_deps <- setdiff(all_adam_dep, outputs)

  # Return early if there are no missing dependencies
  if (length(missing_adam_deps) == 0) {
    return(invisible(NULL))
  }

  # Prepare error message
  idx <- vapply(
    actions$depend_cols,
    function(x) {
      any(paste0(x$domain, ".", x$column_name) %in% missing_adam_deps)
    },
    logical(1)
  )

  # Get outputs of actions affected by the missing dependencies
  actions_missing_deps <- NULL
  if (any(idx)) {
    actions_missing_deps <- lapply(which(idx), function(i) {
      paste0(toupper(actions$domain[[i]]), ".", unlist(actions$output[[i]]))
    }) |>
      unlist() |>
      unique()
  }

  # Get filters affected by the missing dependencies
  domains <- unique(actions$domain)
  filter_missing_deps <- lapply(domains, function(nm) {
    if (any(filter_dep_by_domain[[nm]] %in% missing_adam_deps)) {
      paste(nm, "filter")
    }
  }) |>
    unlist()

  # Combine all affected components
  affected_components <- c(actions_missing_deps, filter_missing_deps)

  # Print error message
  stop(
    "\n\nThe following columns are missing in the ADaM spec:\n\t",
    toupper(paste0(sort(missing_adam_deps), collapse = "\n\t")),
    "\nto execute:\n\t",
    paste0(sort(affected_components), collapse = "\n\t")
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
#'   action dependency information.
#' @param filter_dep_by_domain List mapping domain names to their filter dependencies.
#'
#' @return
#' Invisible NULL if all dependencies are satisfied, or a character string
#' containing a detailed error message listing missing columns and affected
#' actions/filters within the domain.
#'
#' @details
#' The function focuses on internal dependencies within a single domain,
#' identifying missing parent columns that are required by actions or filters
#' in the same domain. Returns formatted error messages for integration into
#' larger validation workflows.
#'
#' @noRd
check_adam_dependencies_within_domain <- function(
  domain,
  adam_dep_by_domain,
  outputs,
  actions_by_domain,
  filter_dep_by_domain
) {
  # Identify missing ADaM column dependencies for the specific domain
  missing_adam_deps <- setdiff(adam_dep_by_domain[[domain]], outputs)

  # Return early if there are no missing dependencies
  if (length(missing_adam_deps) == 0) {
    return(invisible(NULL))
  }

  # Extract missing column dependencies that belong to the specific domain
  missing_adam_deps_domain <- lapply(missing_adam_deps, function(x) {
    str_split <- strsplit(x, "\\.")[[1]]
    if (toupper(str_split[1]) == toupper(domain) & length(str_split) == 2) {
      str_split[2]
    }
  }) |>
    unlist()

  # Return early if there are no missing dependencies within the domain.
  # Otherwise proceed by generating an error message.
  if (length(missing_adam_deps_domain) == 0) {
    return(invisible(NULL))
  }

  # Find which actions are affected by the missing columns
  idx <- vapply(
    actions_by_domain[[domain]]$depend_cols,
    function(y) {
      any(
        y$column_name %in%
          missing_adam_deps_domain &
          toupper(y$domain) == toupper(domain)
      )
    },
    logical(1)
  )

  # Get outputs of actions affected by the missing dependencies
  affected_components <- actions_by_domain[[domain]]$outputs[idx] |>
    unlist() |>
    unique()

  # Get filters affected by the missing dependencies
  filter_has_missing_deps <- any(
    filter_dep_by_domain[[domain]] %in%
      paste0(domain, ".", missing_adam_deps_domain)
  )
  if (filter_has_missing_deps) {
    affected_components <- c(affected_components, paste(domain, "filter"))
  }

  # Create error message
  paste0(
    "\n\nThe following columns are missing in the ",
    toupper(domain),
    " spec:\n\t",
    paste0(
      toupper(domain),
      ".",
      sort(missing_adam_deps_domain),
      collapse = "\n\t"
    ),
    "\nto execute:\n\t",
    paste0(toupper(domain), ".", sort(affected_components), collapse = "\n\t")
  )
}
