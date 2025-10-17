#' @title Arm group 01
#' @description A description
#' @type derivation
#' @depends ADSL PLANNED_ARM
#' @outputs ARM_GRP1
#' @returns `ADSL`
#' @code
arm_group_01 <- function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(
      ARM_GRP1 = ifelse(
        PLANNED_ARM %in% c("Placebo", "Screen Failure"),
        "Placebo or Screen Failure",
        PLANNED_ARM
      )
    )
  return(ADSL)
}
