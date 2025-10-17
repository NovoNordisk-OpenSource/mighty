#' @title Age diff 01
#' @description A description
#' @type derivation
#' @depends ADSL AGE
#' @depends ADSL AGE2
#' @outputs AGE_DIFF1
#' @returns `ADSL`
#' @code
age_diff_01 <- function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE_DIFF1 = AGE - AGE2)
  return(ADSL)
}
