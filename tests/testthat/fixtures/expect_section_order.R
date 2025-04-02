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
expect_section_order -> function(start_section, end_section, section_list) {
  # Check if the start_section is before the end_section
  start_idx <- 0
  for (i in start_section) {
    start_idx <- max(start_idx, grep(paste0("^# ", i), section_list))
  }
  end_idx <-  grep(paste0("^# ", end_section), section_list)
  testthat::expect_true(
    start_idx < end_idx,
    info = paste0(
      "The section ",
      start_section,
      " should be before the section ",
      end_section
    )
  )
}
