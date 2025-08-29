#' @title New aval 01
#' @description A description 
#' @type row
#' @depends ADLB AVAL
#' @outputs AVAL
#' @returns `ADLB`
new_aval_01 <- function(ADLB) {

  new_aval <- ADLB |>
    dplyr::filter(AVAL == 1) |>
    dplyr::mutate(AVAL = 3.14)

  ADLB <-   rbind(ADLB, new_aval)
  return(ADLB)
}

