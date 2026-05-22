#' Build parameters for the `_write_data` template
#'
#' @param .self Character. The name of the domain being written.
#' @param is_final_pgm Logical. `TRUE` when this is the last program for the
#'   domain; `FALSE` for intermediate programs in multi-program domains.
#' @param domain_keys Named list. Primary keys for each domain, used to order
#'   rows before writing.
#' @param domain_ui_data List. Specification metadata for the domain, including
#'   the ordered column list from the YAML spec.
#' @param available_data Data frame or `NULL`. Columns available at this point
#'   in the pipeline, used to prefix not-yet-derived columns with `# `.
#' @param file_ext Character. Output file extension. Defaults to `"parquet"`.
#'
#' @return A named list with `self`, `file_ext`, `row_order_vars`, `keep_vars`.
#'   When `is_final_pgm` is `FALSE`, `keep_vars` is `NULL`. `row_order_vars` is
#'   a `,\n`-separated string, or `NULL` when no keys are defined — whisker
#'   treats `NULL` as falsy so the sort block is suppressed.
#' @noRd
params_write_domain_code <- function(
  .self,
  is_final_pgm,
  domain_keys,
  domain_ui_data,
  available_data,
  file_ext = "parquet"
) {
  keep_vars <- names(domain_ui_data$columns)[
    nchar(names(domain_ui_data$columns)) > 0
  ]
  row_order_vars <- domain_keys[[toupper(.self)]]

  row_order_vars <- if (length(row_order_vars) > 0) {
    paste(row_order_vars, collapse = ",\n")
  } else {
    NULL
  }

  if (is_final_pgm) {
    available_vars <- if (!is.null(available_data)) {
      available_data$column_name[
        available_data$domain == .self & available_data$column_name != "SRC_"
      ]
    } else {
      keep_vars
    }

    has_missing <- !all(keep_vars %in% available_vars)

    keep_vars <- paste(
      ifelse(keep_vars %in% available_vars, keep_vars, paste0("# ", keep_vars)),
      collapse = ",\n"
    )

    if (has_missing) {
      keep_vars <- paste0("\n", fix_comma(keep_vars), "\n")
    }
  } else {
    keep_vars <- NULL
  }

  return(
    list(
      self = .self,
      file_ext = file_ext,
      row_order_vars = row_order_vars,
      keep_vars = keep_vars
    )
  )
}


#' Remove a trailing comma from the last non-commented line of a string
#'
#' @param str Character. A multi-line string where lines are separated by `\n`.
#' @return The input string with the trailing comma removed from the last line
#'   that is not a comment.
#' @noRd
fix_comma <- function(str) {
  lines <- strsplit(str, "\n")[[1]]
  non_commented <- which(!startsWith(trimws(lines), "#") & trimws(lines) != "")
  if (length(non_commented)) {
    lines[max(non_commented)] <- gsub(",$", "", lines[max(non_commented)])
    paste(lines, collapse = "\n")
  } else {
    str
  }
}
