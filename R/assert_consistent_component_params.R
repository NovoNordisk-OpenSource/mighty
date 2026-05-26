#' Assert component outputs are consistent across parameter values
#'
#' @description
#' Validates that when the same component is used multiple times within a
#' domain with different parameter values, each use produces separate output
#' columns. If two uses share an output column but differ in parameter values,
#' the resulting derivation would be ambiguous.
#'
#' The check is scoped per domain: the same component reused across different
#' domains with different parameter values is allowed, because each domain's
#' outputs are independent.
#'
#' @details
#' This check runs before [assert_code_outputs_in_yaml()] so that the more
#' specific "different parameter values" error is raised instead of a generic
#' YAML/code mismatch.
#'
#' Only components of type `"column"` are checked, consistent with
#' [assert_code_outputs_in_yaml()]. Row and parameter components are excluded
#' because their `@outputs` tag names an existing column they operate on, not a
#' new column — so overlapping output names across parameter sets are intentional.
#'
#' @param x A `data.table` produced by `merge()` inside
#'   `merge_ui_with_component_metadata()`, containing at least:
#'   - `domain`: character
#'   - `code_id`: character (NA for non-component actions)
#'   - `type_from_code`: character (`"column"`, `"row"`, `"parameter"`, `"internal"`)
#'   - `outputs_from_code`: list column of character vectors
#'   - `parameters`: list column of named lists
#'
#' @return Invisibly `TRUE` if valid, otherwise aborts with a cli error
#'   whose message contains "different parameter values".
#' @noRd
assert_consistent_component_params <- function(x) {
  components <- get_column_components(
    x,
    c("domain", "code_id", "outputs_from_code")
  )

  if (nrow(components) == 0) {
    return(invisible(TRUE))
  }

  violations <- find_overlapping_outputs(components)

  if (nrow(violations) == 0) {
    return(invisible(TRUE))
  }

  error_details <- vapply(
    seq_len(nrow(violations)),
    \(i) format_overlap_violation(violations[i]),
    character(1)
  )
  names(error_details) <- rep("i", length(error_details))

  throw_validation_error(
    category = "Overlapping component outputs",
    details = error_details,
    suggestions = c(
      "Ensure each use of the same component produces different output columns",
      "Use distinct @outputs (via template parameters) for each set of parameter values"
    )
  )
}


#' Get column-type components
#'
#' Shared helper used by [assert_consistent_component_params()] and
#' [assert_code_outputs_in_yaml()]. Filters to rows with a non-NA `code_id`
#' and `type_from_code == "column"`, selects the requested columns plus
#' `parameters`, and adds a `parameters_hashed` column for grouping by unique parameter values.
#'
#' @param x A `data.table` produced by `merge_ui_with_component_metadata()`.
#' @param cols Character vector of column names to keep (in addition to
#'   `parameters`, which is always included).
#' @return A `data.table` with the selected columns and `parameters_hashed`.
#'   Returns a zero-row table when no column-type components are present.
#' @noRd
get_column_components <- function(x, cols) {
  components <- x[
    !is.na(code_id) & type_from_code == "column",
    .SD,
    .SDcols = unique(c(cols, "parameters"))
  ]
  if (nrow(components) == 0) {
    return(components)
  }
  components[,
    parameters_hashed := vapply(parameters, rlang::hash, character(1))
  ]
  components
}


#' Find overlapping outputs across parameter values within each domain
#'
#' For each `(domain, code_id)` group that has more than one distinct set of
#' parameter values, identifies output columns that appear under multiple
#' parameter values. Cross-domain reuse is not flagged.
#'
#' @param components A `data.table` returned by [get_column_components()]
#'   with at least `domain`, `code_id`, `outputs_from_code`, and
#'   `parameters_hashed`.
#' @return A `data.table` with columns `domain`, `code_id`, and `overlapping`
#'   (list column of character vectors). Only rows with non-empty overlaps are
#'   returned.
#' @noRd
find_overlapping_outputs <- function(components) {
  # Aggregate outputs per (domain, code_id, parameters_hashed)
  by_param <- components[,
    .(outputs = list(unique(unlist(outputs_from_code)))),
    by = .(domain, code_id, parameters_hashed)
  ]
  # Keep only groups with >1 distinct parameter set
  by_param <- by_param[, if (.N > 1) .SD, by = .(domain, code_id)]
  if (nrow(by_param) == 0) {
    return(data.table(
      domain = character(0),
      code_id = character(0),
      overlapping = list()
    ))
  }
  # Find outputs appearing in multiple parameter sets
  violations <- by_param[,
    .(overlapping = {
      all_out <- unlist(outputs)
      list(unique(all_out[duplicated(all_out)]))
    }),
    by = .(domain, code_id)
  ]
  violations[lengths(overlapping) > 0]
}


#' Format a single overlap violation for display
#'
#' @param violation A single-row `data.table` with columns `domain`, `code_id`,
#'   and `overlapping`.
#' @return A single formatted string.
#' @noRd
format_overlap_violation <- function(violation) {
  display_id <- display_component_id(violation$code_id)
  overlapping <- violation$overlapping[[1]]
  domain <- violation$domain
  col_str <- format_list(toupper(overlapping), format_column)
  paste0(
    format_domain(domain),
    " - ",
    cli::format_inline("{.file {display_id}}"),
    ": ",
    col_str,
    " would be derived with different parameter values"
  )
}


#' Format a component ID for display
#'
#' Custom components use file paths as their ID. For display purposes, only the
#' file basename is shown (e.g., `path/to/der_complsfl.R` becomes
#' `der_complsfl.R`). Standard components are returned as-is.
#'
#' @param code_id A component identifier string.
#' @return A string suitable for display in user-facing messages.
#' @noRd
display_component_id <- function(code_id) {
  if (grepl("/", code_id)) basename(code_id) else code_id
}
