#' Validate UI YAML Input
#' @description Validates that ui_yml parameter meets expected structure and content requirements
#' @param ui_yml The UI YAML input to validate
#' @return Invisible TRUE if valid, otherwise stops with error
assert_valid_ui_yml <-  function(ui_yml) {

  # Check all elements are named
  if (is.null(names(ui_yml)) || any(names(ui_yml) == "")) {
    stop("All elements in ui_yml must be named (representing domain names)")
  }

  # Check for duplicate domain names
  domain_names <- names(ui_yml)
  if (any(duplicated(domain_names))) {
    duplicated_domains <- domain_names[duplicated(domain_names)]
    stop("Duplicate domain names found in ui_yml: ", paste(duplicated_domains, collapse = ", "))
  }

  # Validate each domain specification
  for (domain_name in domain_names) {
    domain_spec <- ui_yml[[domain_name]]
    assert_valid_domain_spec(domain_spec, domain_name)
  }

  return(invisible(TRUE))
}


#' Validate Individual Domain Specification
#' @description Validates the structure of a single domain specification
#' @param domain_spec The domain specification to validate
#' @param domain_name The name of the domain (for error messages)
#' @return Invisible TRUE if valid, otherwise stops with error
assert_valid_domain_spec <- function(domain_spec, domain_name) {
  # Check basic structure
  if (!is.list(domain_spec)) {
    stop("Domain specification for '", domain_name, "' must be a list, got: ", class(domain_spec)[1])
  }

  # Check required top-level elements
  required_elements <- c("columns", "domain")
  missing_elements <- setdiff(required_elements, names(domain_spec))
  if (length(missing_elements) > 0) {
    stop("Domain specification for '", domain_name, "' is missing required elements: ",
         paste(missing_elements, collapse = ", "))
  }

  # Validate domain name consistency
  if (!identical(domain_spec$domain, domain_name)) {
    stop("Domain name mismatch: list element named '", domain_name,
         "' but domain specification contains domain = '", domain_spec$domain, "'")
  }

  # Validate columns structure
  if (!is.list(domain_spec$columns)) {
    stop("'columns' element for domain '", domain_name, "' must be a list")
  }

  if (length(domain_spec$columns) == 0) {
    stop("'columns' element for domain '", domain_name, "' cannot be empty")
  }

  # Validate each column specification
  for (i in seq_along(domain_spec$columns)) {
    column_spec <-  domain_spec$columns[[i]]
    assert_valid_column_spec(column_spec, domain_name, i)
  }

  # Validate init section if present
  if ("init" %in% names(domain_spec)) {
    assert_valid_init_spec(domain_spec$init, domain_name)
  }

  return(invisible(TRUE))
}

#' Validate Column Specification
#' @description Validates the structure of a single column specification
#' @param column_spec The column specification to validate
#' @param domain_name The domain name (for error messages)
#' @param column_index The index of the column (for error messages)
#' @return Invisible TRUE if valid, otherwise stops with error
assert_valid_column_spec <- function(column_spec, domain_name, column_index) {
  if (!is.list(column_spec)) {
    stop("Column specification ", column_index, " in domain '", domain_name,
         "' must be a list, got: ", class(column_spec)[1])
  }

  # Validate depend_cols structure if present
  if ("depend_cols" %in% names(column_spec) && !is.null(column_spec$depend_cols)) {
    if (!is.character(column_spec$depend_cols) && !is.list(column_spec$depend_cols)) {
      stop("depend_cols in column ", column_index, " of domain '", domain_name,
           "' must be character vector or list")
    }
  }

  # Validate outputs structure if present
  if ("outputs" %in% names(column_spec) && !is.null(column_spec$outputs)) {
    if (!is.character(column_spec$outputs)) {
      stop("outputs in column ", column_index, " of domain '", domain_name,
           "' must be character vector")
    }
    if (length(column_spec$outputs) == 0) {
      stop("outputs in column ", column_index, " of domain '", domain_name,
           "' cannot be empty")
    }
  }

  return(invisible(TRUE))
}

#' Validate Init Specification
#' @description Validates the structure of the init section
#' @param init_spec The init specification to validate
#' @param domain_name The domain name (for error messages)
#' @return Invisible TRUE if valid, otherwise stops with error
assert_valid_init_spec <- function(init_spec, domain_name) {
  if (!is.list(init_spec)) {
    stop("'init' section for domain '", domain_name, "' must be a list, got: ", class(init_spec)[1])
  }

  # Validate filter specifications if present
  filter_fields <- c("filter_global", "filter_domain", "filter_depend_cols")
  for (field in filter_fields) {
    if (field %in% names(init_spec) && !is.null(init_spec[[field]])) {
      if (!is.character(init_spec[[field]]) && !is.list(init_spec[[field]])) {
        stop(field, " in `init` section for domain '", domain_name,
             "' must be character vector or list")
      }
    }
  }

  return(invisible(TRUE))
}
