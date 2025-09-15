#' @title Aval grp 01
#' @description A description
#' @type derivation
#' @depends ADLB AVAL
#' @outputs AVAL_GRP
#' @returns `ADLB`
#' @code
aval_grp_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AVAL_GRP = ifelse(
      AVAL < 10, "low",
      ifelse(AVAL < 100, "medium", "high")
    ))
  return(ADLB)
}

