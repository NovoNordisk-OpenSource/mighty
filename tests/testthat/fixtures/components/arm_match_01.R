#' @title Arm match 01
#' @description A description
#' @type derivation
#' @depends ADSL PLANNED_ARM
#' @depends ADSL ACTARM
#' @outputs ARM_MATCH
#' @code
ADSL <- ADSL |>
  dplyr::mutate(
    ARM_MATCH = ifelse(
      PLANNED_ARM == ACTARM,
      "Match",
      "Mismatch"
    )
  )
