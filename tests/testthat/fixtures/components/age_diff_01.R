#' @title Age diff 01
#' @description A description
#' @type column
#' @depends ADSL AGE
#' @depends ADSL AGE2
#' @outputs AGE_DIFF1
#' @code
ADSL <- ADSL |>
  dplyr::mutate(AGE_DIFF1 = AGE - AGE2)
