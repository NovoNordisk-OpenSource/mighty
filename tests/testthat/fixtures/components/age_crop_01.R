#' @title Age crop 01
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL AGE
#' @outputs AGE2
#' @code
ADSL <- ADSL |>
  dplyr::mutate(AGE2 = ifelse(AGE > 80, 80, AGE))
