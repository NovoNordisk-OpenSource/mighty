#' @title VISFL
#' @description Flag rows whose derived VISIT is "VISIT 3"
#'
#' @type column
#' @origin Derived
#' @method Flag built from VISIT
#' @depends ADLB VISIT
#' @outputs VISFL
#' @code
ADLB <- ADLB |>
  dplyr::mutate(VISFL = ifelse(VISIT == "VISIT 3", "Y", "N"))
