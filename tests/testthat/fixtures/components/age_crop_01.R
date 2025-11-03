#' @title Age crop 01
#' @description A description
#' @type derivation
#' @depends ADSL AGE
#' @outputs AGE2
#' @code
ADSL <- ADSL |>
  dplyr::mutate(AGE2 = ifelse(AGE > 80, 80, AGE))
