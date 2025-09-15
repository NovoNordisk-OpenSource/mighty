#' @title Lbtest_03
#' @description A description
#' @type derivation
#' @depends ADLB LBTEST
#' @outputs LBTEST3
#' @outputs LBTEST3_FLG
#' @returns `ADLB`
#' @code
lbtest_03 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(
      LBTEST3 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      )) |>
    dplyr::mutate(
      LBTEST3_FLG = dplyr::case_when(
        LBTEST3 == "Invalid" ~ "Y",
        LBTEST3 != "Invalid" ~ "Invalid"
      ))
  return(ADLB)
}
