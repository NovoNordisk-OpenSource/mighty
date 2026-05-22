#' Expect Section Order
#'
#' Checks whether the specified start section(s) appear(s) before the end
#' section in the provided section list. This is useful for validating the
#' order of sections in documents or reports.
#'
#' @param start_section A character vector containing the names of sections
#' to check if they precede the end section.
#' @param end_section A character string representing the section that
#' should come after the start section.
#' @param section_list A character vector of sections in the document
#'
#' @return NULL; the function will throw an error if the start section
#' does not come before the end section.
#'
#' @examples
#' sections <- c("# Introduction", "# Methods", "# Results", "# Conclusion")
#' expect_section_order("Introduction", "Results", sections)
#'
#' @import testthat
#'
expect_section_order <- function(start_section, end_section, section_list) {
  # Check if the start_section is before the end_section
  start_idx <- c()

  for (i in start_section) {
    m <- paste0("^# ", i, "\\s?[-]*$") |> grep(section_list)
    testthat::expect_true(
      length(m) > 0,
      info = paste0("The section ", i, " does not exist in the document")
    )
    testthat::expect_true(
      length(m) < 2,
      info = paste0("The section ", i, " exists in multiple places")
    )
    start_idx[i] <- m
  }

  end_idx <- paste0("^# ", end_section, "\\s?[-]*$") |> grep(section_list)

  testthat::expect_true(
    length(end_idx) > 0,
    info = paste0(
      "The section ",
      end_section,
      " does not exist in the document"
    )
  )

  testthat::expect_true(
    length(end_idx) < 2,
    info = paste0(
      "The section ",
      end_section,
      " exists in multiple places"
    )
  )

  inx <- start_idx < end_idx
  for (i in seq_along(inx)) {
    testthat::expect_true(
      inx[i],
      info = paste0(
        "The section <",
        names(inx[i]),
        "> should be before the section <",
        end_section,
        ">"
      )
    )
  }
}


#' Extract and Normalize CLI Error Text
#'
#' Combines `$message` and `$body` from a `cli::cli_abort()` error condition
#' into a single whitespace-normalized string, suitable for pattern matching
#' with `expect_match()`.
#'
#' @param err An error condition object (as returned by `expect_error()`)
#' @return A single character string with collapsed whitespace
#'
#' @examples
#' err <- expect_error(some_function())
#' expect_match(get_error_text(err), "expected pattern")
#'
get_error_text <- function(err) {
  if (inherits(err, "purrr_error_indexed")) err <- err$parent
  full <- paste(c(err$message, err$body), collapse = " ")
  gsub("\\s+", " ", full)
}

get_temp_connector_config_path <- function(
  env = parent.frame()
) {
  get_connector_config_path(withr::local_tempdir(.local_envir = env))
}

get_connector_config_path <- function(path) {
  file.path(path, "_connector.yml")
}
