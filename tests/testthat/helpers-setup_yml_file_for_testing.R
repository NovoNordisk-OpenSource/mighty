#' Setup YAML Configuration Files for Testing
#'
#' This function processes YAML configuration files for use in testing
#' environments
#' by creating temporary copies with path interpolation. It reads existing
#' YAML files,
#' processes them with glue templating (substituting `{path_base}` with the
#' test path),
#' and creates temporary files that are automatically cleaned up when the test
#' environment ends.
#'
#' @param config_files A character vector of file paths to YAML configuration
#'   files
#'   that need to be processed for testing.
#' @param test_env The testing environment object (typically from testthat)
#'   where
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
  return(result |> unlist(use.names = FALSE))
}


# Helper function to create temporary YAML files with fixed name
create_temp_yaml <- function(
  content,
  name = "temp_test_file.yml",
  .local_envir = parent.frame()
) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  path <- file.path(dir, name)
  writeLines(content, path)
  path
}


#' Setup Temporary Study Directory for Testing
#'
#' Creates a temporary directory and writes YAML files for testing purposes.
#' The directory is automatically cleaned up when the calling environment exits
#' (typically at the end of a testthat test).
#'
#' @param yaml_list A named list where each element contains YAML content as a
#'   character vector (suitable for `writeLines`). The names of the list
#'   elements will be used as filenames (with .yml extension added if not
#'   present).
#' @param .local_envir The environment where the temporary directory should be
#'   registered for cleanup. Defaults to the parent frame.
#'
#' @return The path to the created temporary directory containing the YAML files.
#'
#' @keywords internal testing
#' @noRd
setup_study_dir <- function(yaml_list, .local_envir = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .local_envir)
  for (name in names(yaml_list)) {
    filename <- if (grepl("\\.yml$", name)) {
      name
    } else {
      paste0(name, ".yml")
    }

    filepath <- file.path(dir, filename)
    writeLines(yaml_list[[name]], filepath)
  }

  return(dir)
}


#' Setup Study Directory from Test Fixtures
#'
#' Convenience function to load YAML fixtures, process glue placeholders,
#' and create a study directory for testing with mighty_study().
#'
#' @param fixtures Named list where names are file IDs (e.g., "adsl", "adlb",
#'   "_mighty") and values are fixture filenames (e.g., "complex_adsl.yml")
#' @param process_glue Logical. If TRUE (default), processes {path_base} glue
#'   placeholders in the YAML files.
#' @param .local_envir Environment for cleanup. Defaults to parent frame.
#'
#' @return Path to temporary study directory containing processed YAML files
#'
#'
#' @noRd
setup_study_from_fixtures <- function(
  fixtures,
  process_glue = TRUE,
  .local_envir = parent.frame()
) {
  path_base <- testthat::test_path()

  yaml_list <- lapply(names(fixtures), function(domain_id) {
    fixture_name <- fixtures[[domain_id]]
    content <- readLines(testthat::test_path("fixtures", fixture_name))

    if (process_glue) {
      processed <- as.character(glue::glue(
        paste(content, collapse = "\n"),
        path_base = path_base,
        .trim = FALSE
      ))
      strsplit(processed, "\n")[[1]]
    } else {
      content
    }
  })
  names(yaml_list) <- names(fixtures)

  setup_study_dir(yaml_list, .local_envir = .local_envir)
}
