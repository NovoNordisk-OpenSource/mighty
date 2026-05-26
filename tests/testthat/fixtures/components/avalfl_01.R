#' @title avalfl_01
#' @description A description
#' @type column
#' @depends ADLB AVAL
#' @outputs AVALFL
#' @outputs AVALREA
#' @code
ADLB <- ADLB |>
  dplyr::mutate(AVALFL = ifelse(is.na(AVAL), "Y", "N")) |>
  dplyr::mutate(AVALREA = ifelse(AVALFL == "Y", "Missing AVAL", ""))
