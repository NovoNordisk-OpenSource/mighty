#' @title age4_01
#' @description A description 
#' @type derivation
#' @depends ADLB AGE2
#' @outputs AGE4
#' @returns `ADLB`
#' @code
age4_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AGE4 = AGE2 - 2)
  return(ADLB)
}

