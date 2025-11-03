#' @title age2_01
#' @description A description
#' @type derivation
#' @depends core AGE
#' @outputs AGE2
#' @code

ADLB <- ADLB |>
  dplyr::mutate(AGE2 = 10 * AGE)
