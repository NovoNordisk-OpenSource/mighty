#' Throw a YAML validation error with consistent formatting
#'
#' @param yaml_file Path to the YAML file
#' @param messages Character vector of error messages to display
#' @param parent Optional parent condition for chaining errors
#' @noRd
throw_yaml_validation_error <- function(yaml_file, messages, parent = NULL) {
  file_display <- basename(yaml_file)
  cli::cli_abort(
    c(
      "x" = "YAML validation failed for {.file {file_display}} with the following error(s):",
      messages
    ),
    class = "yaml_validation_error",
    parent = parent
  )
}
