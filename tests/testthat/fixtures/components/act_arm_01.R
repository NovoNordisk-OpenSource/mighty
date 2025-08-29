#' @title ACTARM
#' @description A description
#' 
#' @type derivation
#' @depends dm STUDYID
#' @depends dm USUBJID
#' @depends dm ACTARM
#' @depends dm_vaccine STUDYID
#' @depends dm_vaccine USUBJID
#' @depends dm_vaccine ACTARM
#' @depends ADSL STUDYID
#' @depends ADSL USUBJID
#' @outputs ACTARM
#' @returns `ADSL`
act_arm_01 <- function(ADSL, dm, dm_vaccine) {
  ADSL <- ADSL |>
    dplyr::left_join(
      rbind(dm |> dplyr::select(STUDYID, USUBJID, ACTARM),
            dm_vaccine |> dplyr::select(STUDYID, USUBJID, ACTARM)),
      by = c("STUDYID", "USUBJID")
    ) |>
    dplyr::mutate(ACTARM = ifelse(
      ACTARM == "Placebo", "PLACEBO", ACTARM
    ))
  return(ADSL)
}
