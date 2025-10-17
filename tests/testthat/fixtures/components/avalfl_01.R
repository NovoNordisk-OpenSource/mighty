#' @title avalfl_01
#' @description A description
#' @type derivation
#' @depends ADLB AVAL
#' @outputs AVALFL
#' @outputs AVALREA
#' @returns `ADLB`
#' @code
avalfl_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AVALFL = ifelse(is.na(AVAL), "Y", "N")) |>
    dplyr::mutate(AVALREA = ifelse(AVALFL == "Y", "Missing AVAL", ""))
  return(ADLB)
}
