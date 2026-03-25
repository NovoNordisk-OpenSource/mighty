#' CLI Formatting Helpers
#'
#' @description
#' Semantic color formatting for validation error messages.
#'
#' @details
#' **Color scheme:**
#' - `format_domain()`: Magenta (ADSL, ADLB)
#' - `format_column()`: Green (AGE, USUBJID) - for YAML/component definitions
#' - `format_qualified_column()`: Cyan (ADSL.AGE) - for ADaM dependencies
#' - `format_column_ref()`: Auto-detect - for mixed ADaM/SDTM dependencies
#'
#' **Key distinction - Definitions vs References:**
#'
#' Use `format_column()` for columns being DEFINED:
#' - YAML column specs: `columns: - id: AGE`
#' - Component outputs: `#' @outputs AGE`
#'
#' Use `format_qualified_column()` for ADaM REFERENCES:
#' - ADaM dependencies: `#' @depends ADSL AGE` → stored as "ADSL.AGE"
#' - Validates domain.column format (strict = TRUE)
#'
#' Use `format_column_ref()` for mixed ADaM/SDTM:
#' - ADaM: "ADSL.AGE" (cyan)
#' - SDTM: "BRTHDTC" (green, no domain prefix)
#'
#' @name format_helpers
#' @noRd
NULL

#' Format Domain Name with cli Color
#'
#' @description Applies magenta color to domain names
#'
#' @param x Character vector of domain names (e.g., "ADSL", "ADLB")
#' @return Character vector with ANSI color codes
#' @noRd
#'
#' @examples
#' \dontrun{
#' format_domain("ADSL")  # Returns magenta-colored ADSL
#' }
format_domain <- function(x) {
  cli::col_magenta(x)
}

#' Format Column Name with cli Color
#'
#' @description Green color for bare column names. Use for column definitions
#' (YAML specs, component `@outputs`).
#'
#' @param x Character vector of bare column names (e.g., "AGE", "USUBJID")
#' @return Character vector with ANSI color codes
#' @noRd
format_column <- function(x) {
  cli::col_green(x)
}

#' Format Domain-Qualified Column Reference with cli Color
#'
#' @description Cyan color for domain-qualified references (domain.column).
#' Use for ADaM dependency references. Validates qualification by default.
#'
#' @param x Character vector (e.g., "ADSL.AGE", "ADLB.PARAMCD")
#' @param strict Logical. If TRUE, validates domain.column format. Default: TRUE.
#' @return Character vector with ANSI color codes
#' @noRd
format_qualified_column <- function(x, strict = TRUE) {
  if (strict) {
    # Validate that all non-NA strings contain a period (domain.column)
    invalid <- x[!is.na(x) & !grepl("\\.", x)]
    if (length(invalid) > 0) {
      cli::cli_abort(c(
        "format_qualified_column() requires domain-qualified references (domain.column)",
        "x" = "Found unqualified column name{?s}: {.val {invalid}}",
        "i" = "Use format_column() for bare column names instead",
        "i" = "Or set strict = FALSE to allow mixed qualified/unqualified references"
      ))
    }
  }
  cli::col_cyan(x)
}

#' Format Column Reference (Auto-detect Qualification)
#'
#' @description Auto-detects qualification: cyan for qualified ADaM references
#' (domain.column), green for unqualified SDTM references. Use for mixed
#' ADaM/SDTM dependency lists.
#'
#' @param x Character vector (e.g., "ADSL.AGE", "BRTHDTC")
#' @return Character vector with ANSI color codes
#' @noRd
format_column_ref <- function(x) {
  qualified <- !is.na(x) & grepl(".", x, fixed = TRUE)
  result <- character(length(x))
  result[qualified] <- format_qualified_column(x[qualified], strict = FALSE)
  result[!qualified] <- format_column(x[!qualified])
  cli::ansi_string(result)
}

#' Apply markup and join items with Oxford comma
#' @noRd
format_list <- function(items, markup_fn = identity) {
  checkmate::assert_character(items, min.len = 1)
  checkmate::assert_function(markup_fn)

  cli::ansi_collapse(markup_fn(items), sep = ", ", last = ", and ")
}
