#' age3_01
#' 
#' @type col_compute
#' @depends ADLB AGE
#' @outputs AGE3
#' @returns `ADLB`
age3_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AGE3 = AGE - 1)
  return(ADLB)
}

