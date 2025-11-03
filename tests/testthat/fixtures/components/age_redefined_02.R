#' @title age_redefined_02
#' @description A description
#' @type derivation
#' @depends core AGE
#' @outputs AGE
#' @code
ADLB <- ADLB |>
  dplyr::mutate(AGE = AGE + 1)
