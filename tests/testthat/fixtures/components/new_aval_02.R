#' @title New aval 02
#' @description A description
#' @type row
#' @depends ADLB AVAL
#' @outputs AVAL
#' @code
new_aval <- ADLB |>
  dplyr::filter(AVAL == 3.14) |>
  dplyr::mutate(AVAL = 0)

if (nrow(new_aval) == 0) {
}

ADLB <- rbind(ADLB, new_aval)
