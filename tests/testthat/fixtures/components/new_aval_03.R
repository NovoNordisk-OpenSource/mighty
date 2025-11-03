#' @title New aval 03
#' @description A description
#' @type row
#' @depends ADLB AVAL
#' @depends ADLB DOMAIN
#' @outputs AVAL
#' @code
new_aval <- ADLB |>
  dplyr::filter(AVAL == 2 & DOMAIN == "LB") |>
  dplyr::mutate(AVAL = 0)

if (nrow(new_aval) == 0) {
}

ADLB <- rbind(ADLB, new_aval)
