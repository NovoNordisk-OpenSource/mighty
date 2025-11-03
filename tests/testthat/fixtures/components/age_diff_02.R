#' @title Age diff 02
#' @description A description
#' @type derivation
#' @depends ADSL AGE_DIFF1
#' @depends ADSL PLANNED_ARM
#' @outputs AGE_DIFF2
#' @code
ADSL <- ADSL |>
  dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
