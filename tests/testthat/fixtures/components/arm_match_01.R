#' @title Arm match 01
#' @description A description
#' @type derivation
#' @depends ADSL PLANNED_ARM
#' @depends ADSL ACTARM
#' @outputs ARM_MATCH
#' @returns `ADSL`
#' @code
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
