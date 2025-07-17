#' age2_01
#' 
#' @type col_compute
#' @depends core AGE
#' @outputs AGE2
#' @returns `ADLB`
age2_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AGE2 = 10*AGE)
  return(ADLB)
}

