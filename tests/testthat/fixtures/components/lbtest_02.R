#' @title lbtest_02
#' @description A description
#' @type column
#' @depends ADLB LBTEST
#' @outputs LBTEST2
#' @code
ADLB <- ADLB |>
  dplyr::mutate(
    LBTEST2 = dplyr::case_when(
      !is.na(LBTEST) ~ LBTEST,
      is.na(LBTEST) ~ "Invalid"
    )
  )
