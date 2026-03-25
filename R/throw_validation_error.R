#' Throw Validation Error
#'
#' @description Throws a formatted specification validation error with consistent
#' structure across all validation failures.
#'
#' @param category Character string. Error category shown in brackets
#'   (e.g., "Missing dependencies", "Unconnected nodes", "Topology errors")
#' @param details Character vector. Specific error details to display.
#'   Each element becomes a line in the error message. Named elements with "i", "x",
#'   or "!" are formatted as info, error, or warning bullets by cli.
#' @param suggestions Character vector. Suggestion lines displayed as bullet
#'   points. Supports cli markup (e.g., `{.file _mighty.yml}`). Bullet
#'   formatting is applied automatically; do not include `"- "` prefixes.
#'
#' @return Does not return (calls cli::cli_abort)
#' @noRd
#'
#' @examples
#' \dontrun{
#' # Simple error
#' throw_validation_error(
#'   category = "Missing dependencies",
#'   details = c("i" = "ADSL.AGE (required by ADLB.AGE)"),
#'   suggestions = c(
#'     "Add missing columns to their respective domain specifications",
#'     "Check for typos in column names"
#'   )
#' )
#'
#' # Error with multiple details
#' throw_validation_error(
#'   category = "Unconnected nodes",
#'   details = c(
#'     "i" = "ADaM specifications contain 2 inconsistent declarations",
#'     "i" = "Mighty cannot create the necessary dependency relationships",
#'     "",
#'     "Affected nodes:",
#'     node_descriptions
#'   ),
#'   suggestions = c(
#'     "Ensure all dependencies reference columns from the same domain",
#'     "Verify all required domains are included"
#'   )
#' )
#' }
throw_validation_error <- function(category, details, suggestions) {
  checkmate::assert_string(category)
  checkmate::assert_character(details, min.len = 1)
  checkmate::assert_character(suggestions, min.len = 1)

  suggestion_bullets <- setNames(suggestions, rep("*", length(suggestions)))

  cli::cli_abort(
    c(
      "Specification validation errors found:",
      "",
      paste0("[", category, "]"),
      details,
      "",
      "{.strong Suggestions:}",
      suggestion_bullets
    ),
    call = NULL
  )
}
