#' Validate that source and code_id are not both populated
#'
#' This rule checks that columns do not have both method and component.id
#' fields populated simultaneously, as this creates ambiguous behavior.
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_method_and_component_id_not_both_populated <- function(
  yaml_content,
  context = list()
) {
  problem_columns <- character(0)
  for (col in yaml_content$columns) {
    if (has_content(col$method) && has_content(col$component$id)) {
      problem_columns <- c(problem_columns, col$id %||% "")
    }
  }

  if (length(problem_columns) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      "The following columns have both `method` and `component.id` field populated: ",
      paste0("  - ", problem_columns)
    )
  )
}


#' Generic validation for duplicate IDs across sections
#'
#' This function provides a unified approach to validate that IDs are unique
#' across one or more YAML sections.
#'
#' @param sections List of YAML sections to check for duplicates
#' @param error_msg_template Template for error message (e.g., "columns", "row id(s)")
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_duplicate_ids <- function(
  sections,
  error_msg_template,
  context = list()
) {
  sections_with_content <- Filter(has_content, sections)
  if (length(sections_with_content) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  all_ids <- sections_with_content |>
    lapply(extract_ids_from_array) |>
    unlist()

  duplicates <- all_ids[duplicated(all_ids)]

  if (length(duplicates) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      glue::glue(
        "The following {error_msg_template} are defined multiple times:"
      ),
      paste0("  - ", duplicates)
    )
  )
}

#' Validate that column names are unique
#'
#' This rule checks that no column names are duplicated within the
#' columns section of the raw YAML data.
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_duplicate_columns <- function(yaml_content, context = list()) {
  val_no_duplicate_ids(
    sections = list(yaml_content$columns),
    error_msg_template = "columns",
    context = context
  )
}

#' Validate that row/parameter action IDs are unique
#'
#' This rule checks that no row action IDs are duplicated within the
#' rows and parameters sections of the raw YAML data.
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_duplicate_row_parameter_ids <- function(yaml_content, context = list()) {
  val_no_duplicate_ids(
    sections = list(yaml_content$rows, yaml_content$parameters),
    error_msg_template = "row or parameter id(s)",
    context = context
  )
}

#' @noRd
extract_ids_from_array <- function(array) {
  if (!has_content(array)) {
    return(character(0))
  }
  vapply(array, \(x) x$id %||% "", character(1))
}

#' Validate that all row dependencies are defined
#'
#' This rule checks that all row actions referenced in column and row dependencies
#' are actually defined in the rows or parameters sections.
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_depend_rows <- function(yaml_content, context = list()) {
  all_objects <- c(
    yaml_content$columns,
    yaml_content$rows,
    yaml_content$parameters
  )
  all_row_depends <- all_objects |>
    lapply(extract_row_dependencies_from_object) |>
    unlist()

  defined_row_actions <- c(
    extract_ids_from_array(yaml_content$rows),
    extract_ids_from_array(yaml_content$parameters)
  )

  missing_row_actions <- setdiff(all_row_depends, defined_row_actions)
  if (length(missing_row_actions) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      paste(
        "The following row actions are not defined,",
        "but are listed as row dependencies for either a column or another row action:"
      ),
      paste0("  - ", missing_row_actions)
    )
  )
}

#' Extract row dependencies from a single object
#'
#' Helper function that extracts row dependencies (with "rows." or "parameters."
#' prefixes) from a single object's depends field.
#'
#' @param obj A single object (column, row, or parameter) from YAML content
#' @return Character vector of row IDs (with prefixes removed)
#' @noRd
extract_row_dependencies_from_object <- function(obj) {
  if (is.null(obj$depends)) {
    return(character(0))
  }
  row_deps <- obj$depends[is_row_dependency(obj$depends)]
  extract_dependency_id(row_deps)
}


#' Validate that all keys are defined in columns section
#'
#' This rule checks that all columns listed in the keys field
#' are also defined in the columns section. This is need because
#' if a key column is missing in the columns definition it can
#' cause bugs
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_keys_included_as_columns <- function(yaml_content, context = list()) {
  keys <- yaml_content$keys

  defined_columns <- extract_ids_from_array(yaml_content$columns)

  missing_keys <- setdiff(keys, defined_columns)

  if (length(missing_keys) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      "The following columns are specified as keys but are not defined in the columns section:",
      paste0("  - ", missing_keys)
    )
  )
}
