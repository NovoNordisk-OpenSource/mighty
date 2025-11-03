#' @title New visitnum 02
#' @description A description
#' @type row
#' @depends ADLB VISITNUM
#' @outputs VISITNUM
#' @code
new_visitnum <- ADLB |>
  dplyr::filter(VISITNUM == 1.4) |>
  dplyr::mutate(VISITNUM = 1.5)

if (nrow(new_visitnum) == 0) {
}

ADLB <- rbind(ADLB, new_visitnum)
