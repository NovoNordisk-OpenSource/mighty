#' Setup YAML Configuration Files for Testing
#'
#' This function processes YAML configuration files for use in testing environments
#' by creating temporary copies with path interpolation. It reads existing YAML files,
#' processes them with glue templating (substituting `{path_base}` with the test path),
#' and creates temporary files that are automatically cleaned up when the test
#' environment ends.
#'
#' @param config_files A character vector of file paths to YAML configuration files
#'   that need to be processed for testing.
#' @param test_env The testing environment object (typically from testthat) where
#'   the temporary files should be registered for automatic cleanup.
#'
#' @return A character vector of file paths to the created temporary YAML files.
#'   The vector contains the full paths to the temporary files in the same order
#'   as the input `config_files`.
#' @keywords internal testing
#' @noRd
setup_yml_file_for_testing <- function(config_files, test_env) {
  path_base <- testthat::test_path()
  result <- list()

  for (i in seq_along(config_files)) {
    config_file <- config_files[[i]]
    temp_file <- withr::local_tempfile(
      fileext = ".yml",
      .local_envir = test_env
    )
    yaml_text <- readLines(config_file, warn = FALSE)
    yaml_text <- paste(yaml_text, collapse = "\n")
    processed_yaml <- glue::glue(yaml_text, path_base = path_base)

    writeLines(as.character(processed_yaml), temp_file)
    result[[basename(config_file)]] <- temp_file
  }
  return(result |> unlist(use.names = F))
}
