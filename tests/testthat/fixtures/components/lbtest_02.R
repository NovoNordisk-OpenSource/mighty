#' lbtest_02
#' @type col_compute
#' @depends ADLB LBTEST
#' @outputs LBTEST2
#' @returns `ADLB`
lbtest_02 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(
      LBTEST2 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      ))
  return(ADLB)
}
