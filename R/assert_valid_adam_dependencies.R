#' Assert ADaM depdendencies are valid
#' @description Validates that all required parent columns are available for each ADaM domain.
#'
#' @details This function performs comprehensive dependency checking across ADaM domains
#' to ensure data integrity. It operates in two modes:
#'
#' 1. When `check_cross_domain_adam_dependencies = TRUE`: Checks that all dependencies across domains are
#'    satisfied, including external dependencies between different ADaM domains.
#'
#' 2. When `check_cross_domain_adam_dependencies = FALSE`: Only checks that internal dependencies within
#'    each ADaM domain are satisfied.
#'
#' The function identifies dependencies from two sources:
#' - ADSL columns used in domain filters
#' - Column dependencies from actions within each domain
#'
#' If any required parent columns are missing, the function will stop execution with
#' a detailed error message indicating which columns are missing and which actions
#' or filters require those columns.
#'
#' @param x Data frame containing dependency information with columns for 'domain',
#'   'depend_cols', 'outputs', and optionally 'type'
#' @param ui_init List containing UI initialization data with filter dependencies
#'   per domain
#' @param domain_keys Named list mapping domain names to their respective key columns
#' @param check_cross_domain_adam_dependencies Logical indicating whether to check for dependencies
#'   across different ADaM domains (TRUE) or only within each domain (FALSE)
#'
#' @returns Nothing if all dependencies are satisfied; stops with an error message
#'   if dependencies are missing
#'
assert_valid_adam_dependencies <- function(x, ui_init, domain_keys, check_cross_domain_adam_dependencies){

  # Split by ADaM domain
  x_by_domain <- split(x, by = "domain")

  # Find ADSL column dependencies per ADaM domain filter
  domains <- names(ui_init)
  adsl_filter_dep_by_domain <- lapply(domains, get_adsl_filter_dependencies,
                                      ui_init = ui_init, domain_keys = domain_keys)
  names(adsl_filter_dep_by_domain) <- domains

  # Identify column dependencies per ADaM domain from two sources:
  #   - ADSL in domain filter
  #   - actions within the domain
  adam_dep_by_domain <- lapply(x_by_domain, get_all_adam_dependencies,
                               adsl_filter_dep_by_domain)

  # Identify outputs
  outputs <- prefix_outputs_with_domain_for_non_row_nodes(x)


  if (check_cross_domain_adam_dependencies) {
    check_adam_dependencies_cross_domain(x,
                                         adam_dep_by_domain,
                                         outputs,
                                         domains,
                                         adsl_filter_dep_by_domain)
  } else {
    # Only check that the are no missing internal parents per ADaM domain

    error_msg <- c()
    for (nm in domains) {
      error_msg <- c(
        error_msg,
        check_adam_dependencies_within_domain(nm, adam_dep_by_domain, outputs, x_by_domain)
      )
    }

    # If there are any missing dependencies, stop execution
    if (length(error_msg) > 0) {
      stop(error_msg)
    }
  }
}


#' Extract ADSL Filter Dependencies for a Domain
#' @description Extracts ADSL column dependencies from filter conditions for a
#' specific domain.
#'
#' @details Identifies all ADSL columns that are referenced in
#' filter conditions for a given domain. It also ensures that the ADSL domain
#' key is included in the dependencies list.
#'
#' @param nm Character string specifying the domain name to extract filter
#'   dependencies for
#' @param ui_init List containing UI initialization data with filter
#'   dependencies per domain
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns
#'
#' @returns A character vector of ADSL column dependencies for the specified
#'   domain, including both explicitly referenced columns and required key
#'   columns.
get_adsl_filter_dependencies <- function(nm, ui_init, domain_keys) {
  # Get all filter dependencies for the domain
  filter_depend_cols <- ui_init[[nm]]$filter_depend_cols

  # Early return if no filter dependencies exist
  if (length(filter_depend_cols) == 0) {
    return(character(0))
  }

  # Extract only ADSL dependencies (columns starting with "AD")
  filter_depend_cols_adsl <- filter_depend_cols[grepl("^ADSL", filter_depend_cols, ignore.case = TRUE)]

  # Early return if no ADSL dependencies exist
  if (length(filter_depend_cols_adsl) == 0) {
    return(character(0))
  }

  # Extract the ADSL domain name from the first dependency
  adsl_name <- strsplit(filter_depend_cols_adsl[1], "\\.")[[1]][1]

  # Add the ADSL key column to dependencies and ensure uniqueness
  filter_depend_cols_adsl <- unique(c(
    filter_depend_cols_adsl,
    paste0(adsl_name, ".", domain_keys[["ADSL"]])
  ))

  return(filter_depend_cols_adsl)
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
get_adam_dependencies_from_actions <- function(domain_table, depend_col) {
  # Filter for dependencies from ADaM domains
  adam_deps <- depend_col[domain_type %in% c("adam", "init"),]
  if (nrow(adam_deps) == 0) {
    return(character(0))
  }
  adam_deps[, paste0(domain, ".", column_name)]
}

#' Get All ADaM Dependencies for a Domain
#' @description Identifies all ADaM column dependencies for a specific domain
#' from both filter conditions and action dependencies.
#'
#' @details Combines two sources of dependencies: 1. ADSL column
#' dependencies used in domain filters 2. Column dependencies from actions
#' within the domain
#'
#' The result is a unique set of all column dependencies needed for the domain.
#'
#' @param x Data frame or list containing domain information with 'domain' and
#'   'depend_cols' elements
#' @param adsl_filter_dep_by_domain List mapping domain names to their ADSL
#'   filter dependencies
#'
#' @returns A character vector of unique column dependencies for the domain
get_all_adam_dependencies <- function(x, adsl_filter_dep_by_domain) {
  # Find ADSL column dependencies in domain filter
  filter_depend_cols_adsl <- adsl_filter_dep_by_domain[[x$domain[[1]]]]

  # Find column dependencies in actions
  depend_cols <- purrr::map2(x$domain, x$depend_cols, get_adam_dependencies_from_actions) |>
    unlist()

  # Combine the two sources and return set of unique column dependencies
  c(filter_depend_cols_adsl, depend_cols) |>
    unique()
}


#' Prefix Outputs with Domain for Non-Row Nodes
#' @description
#' Creates fully specified column identifiers by prefixing output columns with their domain names,
#' excluding any entries marked as row type.
#'
#' @details
#' Processes a data.table containing domain information and output columns,
#' filtering out any entries marked as "row" type. It generates standardized column identifiers
#' in the format "domain.column_name" that can be used for dependency tracking and validation
#' across different ADaM domains.
#'
#' @param x data.table containing columns 'domain', 'outputs', and 'type'
#'
#' @returns Character vector of domain-prefixed column names in the format "domain.column_name"
prefix_outputs_with_domain_for_non_row_nodes <- function(x) {
  # Filter out rows where type is "row" using data.table syntax
  x_no_rows <- x[is.na(type) | type != "row_compute"]

  # Create fully qualified output names by prefixing with domain
  outputs <- purrr::map2(x_no_rows$domain, x_no_rows$outputs, function(domain, output) {
    paste0(domain, ".", unlist(output))
  }) |> unlist()

  return(outputs)
}

#' Check if Dependency is Missing
#' @description
#' Determines if any column dependencies are in the missing dependencies list.
#'
#' @details
#' Checks if any fully qualified column names (domain.column_name)
#' from the dependency list are found in the missing dependencies list.
#'
#' @param y Data frame or list containing 'domain' and 'column_name' columns
#' @param missing_deps Character vector of missing dependencies
#'
#' @returns Logical value indicating if any dependencies are missing
check_missing_dependency <- function(y, missing_deps) {
  any(paste0(y$domain, ".", y$column_name) %in% missing_deps)
}


#' Find Missing Filter Dependencies
#' @description Identifies domains with filter dependencies that are missing.
#'
#' @details Checks if any ADSL filter dependencies for a domain are in the
#' missing dependencies list. If so, it returns the domain name with " filter"
#' appended.
#'
#' @param nm Character string representing the domain name
#' @param adsl_filter_dep_by_domain List mapping domain names to their ADSL
#'   filter dependencies
#' @param missing_deps Character vector of missing dependencies
#'
#' @returns Character string in the format "domain filter" if missing
#'   dependencies exist, otherwise NULL
find_missing_filter_dependencies <- function(nm, adsl_filter_dep_by_domain, missing_deps) {
  if(any(adsl_filter_dep_by_domain[[nm]] %in% missing_deps)){
    paste0(nm, " filter")
  }
}

#' Check for External ADaM Dependencies
#' @description Checks if all external ADaM dependencies are present in the
#' outputs.
#'
#' @details Verifies that all ADaM column dependencies across domains are
#' included in the outputs. If any dependencies are missing, it generates a
#' detailed error message showing which columns are missing and which actions or
#' filters require those columns.
#'
#' @param x Data frame containing dependency information
#' @param adam_dep_by_domain List mapping domain names to their ADaM
#'   dependencies
#' @param outputs Character vector of available outputs
#' @param domains Character vector of domain names
#' @param adsl_filter_dep_by_domain List mapping domain names to their ADSL
#'   filter dependencies
#'
#' @returns Nothing if all dependencies are present; stops with an error message
#'   if dependencies are missing
check_adam_dependencies_cross_domain <- function(x,
                                                 adam_dep_by_domain,
                                                 outputs,
                                                 domains,
                                                 adsl_filter_dep_by_domain) {
  # If check_external_adam = TRUE then check that there are no missing ADaM
  # column dependencies across domains
  all_adam_dep <- adam_dep_by_domain |> unlist() |> as.character()
  missing_deps <-  setdiff(all_adam_dep, outputs)

  if (length(missing_deps) == 0) {
    return(invisible(NULL))
  }

  # Prepare error message
  idx <-  vapply(x$depend_cols, function(y) {
    check_missing_dependency(y, missing_deps)
  }, logical(1))

  # Initialize actions_missing_deps
  col_missing_deps <-  NULL

  # Get affected actions if any exist
  if (any(idx)) {
    col_missing_deps <- lapply(which(idx), function(i) {
      paste0(x$domain[[i]], ".", unlist(x$output[[i]]))
    }) |> unlist() |> unique()
  }

  # Find missing filter dependencies
  filter_missing_deps <- lapply(domains, function(nm) {
    find_missing_filter_dependencies(nm, adsl_filter_dep_by_domain, missing_deps)
  }) |> unlist()

  # Combine all affected outputs
  outputs_missing_deps <- c(col_missing_deps, filter_missing_deps)

  # Print error message
  stop(
    "\n\nThe following columns are missing in the ADaM spec:\n\t",
    paste0(sort(missing_deps), collapse = "\n\t"),
    "\nto execute:\n\t",
    paste0(sort(outputs_missing_deps), collapse = "\n\t")
  )
}


#' Check for Missing Internal Parent Columns in ADaM Domain
#' @description Checks if all required internal parent columns within a specific
#' ADaM domain are present.
#'
#' @details Examines a specific domain to ensure that all required
#' parent columns within that domain are available in the outputs. If any
#' required parent columns are missing, it generates a detailed error message
#' showing which columns are missing and which actions require those columns.
#'
#' @param nm Character string representing the domain name to check
#' @param adam_dep_by_domain List mapping domain names to their ADaM
#'   dependencies
#' @param outputs Character vector of available outputs
#' @param x_by_domain List of data frames split by domain containing dependency
#'   information
#'
#' @returns Error message as a string if any dependencies are missing, otherwise
#'   NULL
check_adam_dependencies_within_domain <- function(nm,
                                                  adam_dep_by_domain,
                                                  outputs,
                                                  x_by_domain) {
  # Find missing dependencies for this domain
  missing_deps <- setdiff(adam_dep_by_domain[[nm]], outputs)
  if (length(missing_deps) == 0) {
    return(invisible(NULL))
  }

  # Extract only the column names from missing dependencies that belong to this domain
  missing_internal_parent_cols <- lapply(missing_deps, function(mp) {
    str_split <- strsplit(mp, "\\.")[[1]]
    if (str_split[1] == nm) {
      str_split[2]
    }
  }) |> unlist()

  # If there are missing internal parent columns, generate an error, otherwise
  # return early
  if (length(missing_internal_parent_cols) == 0) {
    return(invisible(NULL))
  }

  # Find which actions are affected by the missing columns
  idx <- vapply(x_by_domain[[nm]]$depend_cols, function(y) {
    any(y$column_name %in% missing_internal_parent_cols & y$domain == nm)
  }, logical(1))

  # Get the names of the affected actions
  col_missing_deps <-  x_by_domain[[nm]]$outputs[idx] |>
    unlist() |>
    unique()

  # Create error message
  paste0(
    "\n\nThe following columns are missing in the ", toupper(nm), " spec:\n\t",
    paste0(nm, ".", sort(missing_internal_parent_cols), collapse = "\n\t"),
    "\nto execute:\n\t",
    paste0(nm, ".", sort(col_missing_deps), collapse = "\n\t")
  )
}
