#' @title New aval 02
#' @description A description 
#' @type row_compute
#' @depends ADLB AVAL
#' @outputs AVAL
#' @returns `ADLB`
new_aval_02 <- function(ADLB) {

  new_aval <- ADLB |>
    dplyr::filter(AVAL == 3.14) |>
    dplyr::mutate(AVAL = 0)

  if(nrow(new_aval) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_aval)
  return(ADLB)
}

