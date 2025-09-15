#' @title New aval 04
#' @description A description 
#' @type row
#' @depends ADLB AVAL
#' @depends ADLB AVALC
#' @outputs AVAL
#' @returns `ADLB`
#' @code
new_aval_04 <- function(ADLB) {

  new_aval <- ADLB |>
    dplyr::filter(AVAL > 1000 & !is.na(AVAL) & AVALC != "") |>
    dplyr::mutate(AVAL = 1000)

  if(nrow(new_aval) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_aval)
  return(ADLB)
}

