#' Validate that parameters are not provided when code_id is missing
#'
#' This rule checks that when a code_id field is missing or empty,
#' no parameter fields are provided in the same object.
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_params_when_missing_code_id <- function(yaml_data, context = list()) {
  inx <- vapply(
    yaml_data$columns,
    function(i) {
      any(!is.na(i$parameters)) && is.null(i$code_id)
    },
    logical(1)
  )
  problems <- names(yaml_data$columns)[inx]

  if (length(problems) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      glue::glue("The following columns have parameters but no code_id:"),
      paste0("  - ", unlist(problems))
    )
  )
}

#' Validate that column names are unique
#'
#' This rule checks that no column names are duplicated within the
#' column_action section of the YAML data.
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_duplicate_columns <- function(yaml_data, context = list()) {
  # Filter out empty string names (which are row actions)
  col_names <- names(yaml_data$columns)[names(yaml_data$columns) != ""]
  column_duplicates <- col_names[duplicated(col_names)]

  if (length(column_duplicates) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      glue::glue("The following columns are defined multiple times:"),
      paste0("  - ", column_duplicates)
    )
  )
}

#' Validate that row action IDs are unique
#'
#' This rule checks that no row action IDs are duplicated within the
#' row_action section of the YAML data.
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_no_duplicate_row_ids <- function(yaml_data, context = list()) {
  # Extract id field from unnamed entries (empty string names)
  row_entries <- yaml_data$columns[names(yaml_data$columns) == ""]
  row_ids <- vapply(row_entries, function(x) x$id, character(1))
  row_duplicates <- row_ids[duplicated(row_ids)]

  if (length(row_duplicates) == 0) {
    return(list(valid = TRUE, errors = character(0)))
  }

  list(
    valid = FALSE,
    errors = c(
      glue::glue("The following row id(s) are defined multiple times:"),
      paste0("  - ", row_duplicates)
    )
  )
}

#' Validate that all row dependencies are defined
#'
#' This rule checks that all row actions referenced in depend_rows
#' are actually defined in the row_action section.
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_depend_rows <- function(yaml_data, context = list()) {
  # Get all depend_rows from all entries in columns (both named and unnamed)
  all_row_depends <- lapply(yaml_data$columns, `[[`, "depend_rows") |>
    unlist() |>
    (\(x) x[!is.na(x)])() # Filter out NA values

  # Extract id field from unnamed entries (empty string names) to get defined row actions
  row_entries <- yaml_data$columns[names(yaml_data$columns) == ""]

  if (length(row_entries) == 0) {
    defined_row_actions <- character(0)
  } else {
    defined_row_actions <- vapply(row_entries, function(x) x$id, character(1))
  }

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

#' Validate that source and code_id are not both populated
#'
#' This rule checks that columns do not have both source and code_id
#' fields populated simultaneously, as this creates ambiguous behavior.
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_source_and_code_id_notboth_populated <- function(
  yaml_data,
  context = list()
) {
  # Only check named entries (columns), not unnamed entries (rows)
  column_entries <- yaml_data$columns[names(yaml_data$columns) != ""]

  inx <- vapply(
    column_entries,
    function(i) !is.null(i$code_id) && !is.null(i$depend_cols),
    logical(1)
  )

  if (!any(inx)) {
    return(list(valid = TRUE, errors = character(0)))
  }

  # column names are now the names of the list elements
  problem_columns <- names(column_entries)[inx]

  list(
    valid = FALSE,
    errors = c(
      "The following columns have both `source` and `code_id` field populated: ",
      paste0("  - ", problem_columns)
    )
  )
}


#' Validate that all keys are defined in columns section
#'
#' This rule checks that all columns listed in the keys field
#' are also defined in the columns section. This is need because
#' if a key column is missing in the columns definition it can
#' cause bugs
#'
#' @param yaml_data Parsed YAML data
#' @param context Validation context (yaml_file, ruleset_name, etc.)
#' @return List with 'valid' (logical) and 'errors' (character vector)
#' @noRd
val_keys_included_as_columns <- function(yaml_data, context = list()) {
  keys <- yaml_data$keys

  # Get defined column names (not row actions. Internally, row actions
  # are stored alongside columns in the columns list, but distinguished
  # by having empty string "")
  defined_columns <- names(yaml_data$columns)[names(yaml_data$columns) != ""]

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
