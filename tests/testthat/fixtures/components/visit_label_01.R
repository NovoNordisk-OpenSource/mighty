#' @title VISIT
#' @description Visit label built from the visit number
#'
#' @type column
#' @origin Derived
#' @method Visit label built from VISITNUM
#' @depends ADLB VISITNUM
#' @outputs VISIT
#' @code
ADLB <- ADLB |>
  dplyr::mutate(VISIT = paste("VISIT", VISITNUM))
