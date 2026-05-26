#' @title Aval grp 01
#' @description A description
#' @type column
#' @depends ADLB AVAL
#' @outputs AVAL_GRP
#' @code
ADLB <- ADLB |>
  dplyr::mutate(
    AVAL_GRP = ifelse(
      AVAL < 10,
      "low",
      ifelse(AVAL < 100, "medium", "high")
    )
  )
