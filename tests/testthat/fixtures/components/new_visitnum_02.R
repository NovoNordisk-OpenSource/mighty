#' New visitnum 02
#' 
#' @type row_compute
#' @depends ADLB VISITNUM
#' @outputs VISITNUM
#' @returns `ADLB`
new_visitnum_02 <- function(ADLB) {

  new_visitnum <- ADLB |>
    dplyr::filter(VISITNUM == 1.4) |>
    dplyr::mutate(VISITNUM = 1.5)

  if(nrow(new_visitnum) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_visitnum)
  return(ADLB)
}

