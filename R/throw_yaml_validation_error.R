#' Throw a YAML validation error with consistent formatting
#'
#' @param domain_name Path to the YAML file
#' @param messages Character vector of error messages to display
#' @param parent Optional parent condition for chaining errors
#' @noRd
throw_yaml_validation_error <- function(domain_name, messages, parent = NULL) {
  cli::cli_abort(
    c(
      "x" = "YAML validation failed for {domain_name} with the following error(s):",
      messages
    ),
    class = "yaml_validation_error",
    parent = parent
  )
}
