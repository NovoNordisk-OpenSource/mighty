#' @title age_redefined_01
#' @description A description
#' @type derivation
#' @depends core AGE
#' @depends core SEX
#' @depends ADLB RACE
#' @outputs AGE
#' @code
ADLB <- ADLB |>
  dplyr::mutate(AGE = ifelse(!is.na(SEX) & !is.na(RACE), AGE, 0))
