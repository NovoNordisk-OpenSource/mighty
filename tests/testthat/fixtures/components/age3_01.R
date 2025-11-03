#' @title age3_01
#' @description A description
#' @type derivation
#' @depends ADLB AGE
#' @outputs AGE3
#' @code
ADLB <- ADLB |>
  dplyr::mutate(AGE3 = AGE - 1)
