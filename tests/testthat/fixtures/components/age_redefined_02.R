#' @title age_redefined_02
#' @description A description 
#' @type derivation
#' @depends core AGE
#' @outputs AGE
#' @returns `ADLB`
age_redefined_02 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AGE = AGE + 1)
  return(ADLB)
}

