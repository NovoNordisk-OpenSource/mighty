#' @title New visitnum 01
#' @description A description 
#' @type row_compute
#' @depends ADLB VISITNUM
#' @outputs VISITNUM
#' @returns `ADLB`
new_visitnum_01 <- function(ADLB) {

  new_visitnum <- ADLB |>
    dplyr::filter(round(VISITNUM,2) == 1.3) |>
    dplyr::mutate(VISITNUM = 1.4)

  if(nrow(new_visitnum) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_visitnum)
  return(ADLB)
}

