#' @title Arm match 01
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
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
