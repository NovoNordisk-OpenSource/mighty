#' avalfl_01
#'
#' @type col_compute
#' @depends ADLB AVAL
#' @outputs AVALFL
#' @outputs AVALREA
#' @returns `ADLB`
avalfl_01 <-   function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AVALFL = ifelse(is.na(AVAL), "Y", "N")) |>
    dplyr::mutate(AVALREA = ifelse(AVALFL == "Y", "Missing AVAL", ""))
  return(ADLB)
}
