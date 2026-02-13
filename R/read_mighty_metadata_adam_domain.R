# Main Entry Point

#' Restructure mighty_metadata format to internal ui_yml format
#'
#' @description
#' Transforms an ADaM dataset specification from the mighty_metadata YAML format
#' directly to the internal ui_yml format used in the mighty pipeline.
#'
#' @param yaml_file_path Character string path to a YAML file containing
#'   mighty_metadata specification.
#'
#' @return A named list where the name is the domain name (from the 'id' field),
#'   containing:
#'   \item{columns}{Named list of column specifications, each with outputs,
#'     and optionally depend_cols, code_id, depend_rows, parameters}
#'   \item{domain}{Character string - the domain name}
#'   \item{keys}{Always NULL at this transformation stage}
#'   \item{init}{List containing base_domains, filter_domain, filter_global,
#'     and filter_depend_cols}
#'
#' @examples
#' \dontrun{
#' ui_yml <- read_mighty_metadata_adam_domain("path/to/mighty_metadata.yaml")
#' }
#'
#' @noRd
read_mighty_metadata_adam_domain <- function(yaml_file_path) {
  checkmate::assert_string(yaml_file_path)
  checkmate::assert_file_exists(yaml_file_path)

  yaml_content <- rlang::try_fetch(
    yaml_file_path |>
      mighty.metadata::mighty_metadata() |>
      S7schema::validate_list(
        schema = system.file("schemas", "mighty.json", package = "mighty")
      ) |>
      convert_to_NA_character(),
    error = function(cnd) {
      throw_yaml_validation_error(
        yaml_file_path,
        messages = " ",
        parent = cnd
      )
    }
  )

  checkmate::assert_list(yaml_content)
  checkmate::assert_names(
    names(yaml_content),
    must.include = c("id", "columns")
  )

  # Validate business logic on raw YAML before any transformations
  validate_business_logic(yaml_content, yaml_file = yaml_file_path)

  init <- transform_population_to_init(yaml_content$population, yaml_content$id)

  columns <- transform_columns_to_named_list(
    yaml_content$columns,
    yaml_content$id
  )

  rows <- create_row_action_entries(yaml_content$rows)

  parameters <- create_row_action_entries(yaml_content$parameters)

  all_entries <- c(columns, rows, parameters)

  result <- list(
    columns = all_entries, # Note: 'columns' contains both columns and rows
    domain = yaml_content$id,
    keys = yaml_content$keys,
    init = init
  )

  setNames(list(result), yaml_content$id)
}


# General Utility Helpers

#' Convert "NA" strings to NA_character_ recursively
#'
#' @description
#' Recursively processes a list structure and converts all string "NA" values
#' to proper R NA_character_ values. This ensures consistent handling of missing
#' values throughout the data structure.
#'
#' @param x Any R object - typically a list or character vector
#'
#' @return The same structure as input, with all "NA" strings converted to NA_character_
#'
#' @noRd
convert_to_NA_character <- function(x) {
  # Check if x is a list
  if (is.list(x)) {
    return(lapply(x, convert_to_NA_character))
  }
  # Only process character vectors, avoiding NA/NULL edge cases
  if (is.character(x) && length(x) > 0 && !all(is.na(x))) {
    x[!is.na(x) & x == "NA"] <- NA_character_
  }
  return(x)
}


#' Check if object has meaningful content
#'
#' @description
#' Helper function to check if an object contains meaningful (non-NA/non-NULL) content.
#' For lists, recursively checks if any element has non-NA content in any field.
#' For vectors, checks if any value is not NA.
#' For character vectors, optionally treats empty strings as empty.
#'
#' @param x Any R object to check
#'
#' @return Logical - TRUE if x has meaningful content, FALSE if NULL, empty, or all NA
#'
#' @examples
#' has_content(NULL)                      # FALSE
#' has_content(c(NA, NA))                 # FALSE
#' has_content(c(1, NA))                  # TRUE
#' has_content("")                        # FALSE (default)
#' has_content(list(a = NA, b = NULL))    # FALSE
#' has_content(data.frame(a = c(NA, NA))) # FALSE
#'
#' @noRd
has_content <- function(x) {
  # NULL or length-0 -> no content
  if (is.null(x) || length(x) == 0) {
    return(FALSE)
  }
  if (is.data.frame(x) || is.list(x)) {
    return(
      x |>
        unclass() |>
        vapply(has_content, logical(1)) |>
        any()
    )
  }

  if (is.character(x)) {
    return(
      x |>
        nzchar(keepNA = TRUE) |>
        any(na.rm = TRUE)
    )
  }

  if (is.atomic(x)) {
    return(any(!is.na(x)))
  }

  # Other types: treat presence as content
  TRUE
}

# Extraction Helpers

#' Extract filters from a population entry
#'
#' @description
#' Helper function to extract all filter values from a population entry's
#' filter array. Returns all filters if present, or NA_character_ if empty.
#'
#' @param entry A single population entry with optional filter field
#'
#' @return Character vector of filter expressions, or NA_character_ if no filters
#'
#' @noRd
extract_filters_base <- function(entry) {
  if (has_content(entry$filter)) {
    unlist(entry$filter, use.names = FALSE)
  } else {
    NA_character_
  }
}

extract_filters_global <- function(entry) {
  if (has_content(entry)) {
    lapply(entry, \(x) x$filter) |> unlist()
  } else {
    NA_character_
  }
}


#' Extract parameters from component with field
#'
#' @description
#' Extracts parameters from component$with field, wrapping in list if present
#'
#' @param component_with Component with field from YAML
#'
#' @return List containing parameters, or NA_character_ if none
#'
#' @noRd
extract_component_parameters <- function(component_with) {
  if (has_content(component_with)) {
    list(component_with)
  } else {
    NA_character_
  }
}


# Dependency Collection

#' Collect all dependencies from population base and global sections
#'
#' @description
#' Extracts and consolidates dependencies from both base and global population
#' sections, applying domain prefixing logic for cross-domain dependencies.
#'
#' @param population_base Base population section from YAML
#' @param population_global Global population section from YAML
#' @param domain_id Current domain ID for domain prefix logic
#'
#' @return Character vector of dependencies with appropriate domain prefixes
#'
#' @noRd
collect_all_dependencies <- function(
  population_base,
  population_global,
  domain_id
) {
  # Extract base dependencies
  base_depends <- if (has_content(population_base)) {
    lapply(population_base, function(x) x$depends %||% character(0)) |> unlist()
  } else {
    character(0)
  }

  global_depends <- lapply(population_global, \(x) x$depends) |> unlist()

  all_depends <- c(base_depends, global_depends)
  all_depends[!is.na(all_depends)]
}


# Entry Creation Helpers

#' Determine if method field should be used as depend_cols
#'
#' @description
#' Method needs to be used for any col_echo or col_mutate actions.
#' Checks if a column's method field should be used as depend_cols. The method
#' is used when:
#' 1. No component.id exists (not a col_compute), AND
#' 2. Method references a different column name or different domain than
#'    the current column
#'
#' @param method Method field from column definition (may be NULL)
#' @param component Component object from column definition (may be NULL)
#' @param col_id Current column ID
#' @param domain_id Current domain ID
#'
#' @return Method string if it should be used as depend_cols, NULL otherwise
#'
#' @noRd
should_use_method_as_depend_cols <- function(
  method,
  component,
  col_id,
  domain_id
) {
  # Only use method if no component.id exists (not a col_compute)
  if (has_content(component$id)) {
    return(NULL)
  }

  if (is.null(method)) {
    return(NULL)
  }

  # Method must reference a different column or domain
  method_parts <- strsplit(method, ".", fixed = TRUE)[[1]]

  # Check if method references a different column/domain
  is_different <- if (length(method_parts) == 2) {
    # Format: "DOMAIN.COLUMN"
    method_parts[1] != domain_id || method_parts[2] != col_id
  } else {
    # Format: "COLUMN" (no domain prefix)
    method != col_id
  }

  if (is_different) method else NULL
}


#' Extract row dependencies from column depends field
#'
#' @description
#' Extracts row action dependencies from a column's depends field by filtering
#' for entries with "rows." or "parameters." prefix and removing those prefixes.
#' Both rows and parameters are treated as row actions in mighty's internal model.
#'
#' @param depends Character vector or NULL of dependency strings
#'
#' @return Character vector with "rows." or "parameters." prefix removed, or
#'   NA_character_ if none
#'
#' @noRd
extract_row_deps_from_depends <- function(depends) {
  if (!has_content(depends)) {
    return(NA_character_)
  }
  pattern <- "^(rows|parameters)\\."
  row_deps <- grep(pattern, depends, value = TRUE)
  if (length(row_deps) > 0) {
    sub(pattern, "", row_deps)
  } else {
    NA_character_
  }
}


#' Create a single row action entry
#'
#' @description
#' Creates a standardized row action entry from a row or parameter definition
#'
#' @param entry A single row or parameter entry from mighty_metadata YAML
#'
#' @return List with code_id, id, depend_rows, and parameters
#'
#' @noRd
create_row_action_entry <- function(entry) {
  list(
    code_id = entry$component$id,
    id = entry$id,
    depend_rows = extract_row_deps_from_depends(entry$depends),
    parameters = extract_component_parameters(entry$component$with)
  )
}


# Transformation Functions

#' Transform population structure to init structure
#'
#' @param population The population section from mighty_metadata YAML
#' @param domain_id The ID of the current domain (used to determine if
#'   global dependencies need domain prefix)
#'
#' @return A list with base_domains, filter_domain, filter_global,
#'   and filter_depend_cols
#'
#' @noRd
transform_population_to_init <- function(population, domain_id) {
  if (is.null(population)) {
    return(list())
  }

  init <- list()

  # Extract base domain names
  if (has_content(population$base)) {
    init$base_domains <- vapply(
      population$base,
      function(x) x$domain,
      character(1)
    )
  }

  # Create filter_domain - list of single-key lists, one per base domain
  if (has_content(population$base)) {
    init$filter_domain <- lapply(population$base, function(x) {
      filter_values <- extract_filters_base(x)
      setNames(list(filter_values), x$domain)
    })
  }

  # Extract global filters - flatten all filter arrays
  init$filter_global <- NA_character_
  if (has_content(population$global)) {
    all_filters <- extract_filters_global(population$global)
    all_filters <- all_filters[!is.na(all_filters)]
    if (has_content(all_filters)) {
      init$filter_global <- all_filters
    }
  }

  # Collect all dependencies from both base and global
  all_depends <- collect_all_dependencies(
    population_base = population$base,
    population_global = population$global,
    domain_id = domain_id
  )
  # Always include filter_depend_cols, even if empty (to match old format behavior)
  if (has_content(all_depends)) {
    init$filter_depend_cols <- all_depends
  } else {
    init$filter_depend_cols <- NA_character_
  }
  return(init)
}


#' Transform columns array to named list of column specifications
#'
#' @param columns Array of column definitions from mighty_metadata YAML
#' @param domain_id The ID of the current domain being processed
#'
#' @return Named list where keys are column IDs and values are column specs
#'
#' @noRd
transform_columns_to_named_list <- function(columns, domain_id) {
  if (!has_content(columns)) {
    return(list())
  }

  column_entries <- list()

  for (col in columns) {
    col_id <- col$id

    # Extract row dependencies from depends field (rows. or parameters. prefix)
    depend_rows <- extract_row_deps_from_depends(col$depends)

    # Process method field for column dependencies (col_echo/col_mutate)
    depend_cols <- should_use_method_as_depend_cols(
      col$method,
      col$component,
      col_id,
      domain_id
    )

    # Create base entry with common fields
    entry <- list(
      outputs = col_id,
      depend_rows = depend_rows,
      parameters = extract_component_parameters(col$component$with)
    )

    if (has_content(col$component$id)) {
      # Column with component derivation (col_compute)
      entry$code_id <- col$component$id
    } else if (has_content(depend_cols)) {
      # Column with explicit column dependencies (col_echo/col_mutate)
      entry$depend_cols <- if (length(depend_cols) == 1) {
        depend_cols[[1]]
      } else {
        depend_cols
      }
    }
    # else: Simple column (copied from base domain - col_copy)
    column_entries[[col_id]] <- entry
  }

  return(column_entries)
}


#' Create row action entries from mighty.metadata rows or parameters field
#'
#' @description
#' Converts the `rows` or `parameters` array from mighty.metadata YAML into the
#' internal data model format. Internally, row actions are stored alongside columns
#' in the `columns` list, but distinguished by having empty string `""` as their
#' names (whereas columns use their column ID as the name). Row actions are
#' identified by their `id` field instead of the list name.
#'
#' Both `rows` and `parameters` fields from mighty.metadata are treated as row
#' actions - they add new rows to the dataset rather than derive columns.
#'
#' @param entries Array of row action or parameter definitions from mighty_metadata
#'   YAML
#'
#' @return Named list where keys are empty strings `""` and values contain:
#'   \item{code_id}{Component name or path to the component file if custom}
#'   \item{id}{The row action or parameter identifier}
#'   \item{depend_rows}{Character vector of row IDs this depends on (with
#'     "rows." prefix removed), or NA}
#'   \item{parameters}{List of component parameters from `with` field, or NA}
#'
#' @noRd
create_row_action_entries <- function(entries) {
  if (!has_content(entries)) {
    return(list())
  }
  row_action_key <- "" # Row actions use empty string as key
  lapply(entries, create_row_action_entry) |>
    setNames(rep(row_action_key, length(entries)))
}
