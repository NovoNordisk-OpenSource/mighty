#' @title Arm match 01
#' @description A description 
#' @type col_compute
#' @depends ADSL PLANNED_ARM
#' @depends ADSL ACTARM
#' @outputs ARM_MATCH
#' @returns `ADSL`
arm_match_01 <- function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(
      ARM_MATCH = ifelse(
        PLANNED_ARM == ACTARM,
        "Match",
        "Mismatch"
      )
    )
  return(ADSL)
}
