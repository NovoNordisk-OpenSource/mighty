#' @title Age diff 02
#' @description A description
#' @type derivation
#' @depends ADSL AGE_DIFF1
#' @depends ADSL PLANNED_ARM
#' @outputs AGE_DIFF2
#' @returns `ADSL`
#' @code
age_diff_02 <- function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(ADSL)
}
