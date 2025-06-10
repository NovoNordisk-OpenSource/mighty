#' Well documented node for unit testing
#' @description
#' Documentation of fake function for unit testing roxygen2 methods.
#' This function is supposed to give no errors and a consistent rd documentation.
#'
#' @param .self `data.frame` Input
#' @param x Another input to the function
#' @type derivation
#' @depends .self USUBJID
#' @depends x USUBJID
#' @outputs Y
#' @outputs Z
#' @returns `.self` with added X and Y
#' @export
good <- function(.self, x) {
  TRUE
}
