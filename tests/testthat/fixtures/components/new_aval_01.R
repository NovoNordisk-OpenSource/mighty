#' @title New aval 01
#' @description A description
#' @type row
#' @depends ADLB AVAL
#' @outputs AVAL
#' @code
new_aval <- ADLB |>
  dplyr::filter(AVAL == 1) |>
  dplyr::mutate(AVAL = 3.14)

ADLB <- rbind(ADLB, new_aval)
