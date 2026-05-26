#' @title ACTARM
#' @description A description
#'
#' @type column
#' @depends DM STUDYID
#' @depends DM USUBJID
#' @depends DM ACTARM
#' @depends DM_VACCINE STUDYID
#' @depends DM_VACCINE USUBJID
#' @depends DM_VACCINE ACTARM
#' @depends ADSL STUDYID
#' @depends ADSL USUBJID
#' @outputs ACTARM
#' @code
ADSL <- ADSL |>
  dplyr::left_join(
    rbind(
      DM |> dplyr::select(STUDYID, USUBJID, ACTARM),
      DM_VACCINE |> dplyr::select(STUDYID, USUBJID, ACTARM)
    ),
    by = c("STUDYID", "USUBJID")
  ) |>
  dplyr::mutate(
    ACTARM = ifelse(
      ACTARM == "Placebo",
      "PLACEBO",
      ACTARM
    )
  )
