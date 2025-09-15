#' @title Arm 01
#' @description A description 
#' @type derivation
#' @depends ADSL PLANNED_ARM
#' @depends ADSL USUBJID
#' @outputs NEW_ARM
#' @returns `ADSL`
#' @code
arm_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(NEW_ARM = ifelse(
      USUBJID == "01-701-1015", NA, PLANNED_ARM)
    )
  return(ADSL)
}
