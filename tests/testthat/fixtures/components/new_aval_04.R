#' @title New aval 04
#' @description A description
#' @type row
#' @depends ADLB AVAL
#' @depends ADLB AVALC
#' @outputs AVAL
#' @code
new_aval <- ADLB |>
  dplyr::filter(AVAL > 1000 & !is.na(AVAL) & AVALC != "") |>
  dplyr::mutate(AVAL = 1000)

if (nrow(new_aval) == 0) {
}

ADLB <- rbind(ADLB, new_aval)
