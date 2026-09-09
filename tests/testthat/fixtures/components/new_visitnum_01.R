#' @title New visitnum 01
#' @description A description
#' @type row
#' @origin Derived
#' @method A method description
#' @depends ADLB VISITNUM
#' @outputs VISITNUM
#' @code
new_visitnum <- ADLB |>
  dplyr::filter(round(VISITNUM, 2) == 1.3) |>
  dplyr::mutate(VISITNUM = 1.4)

if (nrow(new_visitnum) == 0) {
}

ADLB <- rbind(ADLB, new_visitnum)
