#' @title supp_dm_01
#' @description A description
#' @type derivation
#' @depends ADSL STUDYID
#' @depends ADSL USUBJID
#' @depends suppdm STUDYID
#' @depends suppdm USUBJID
#' @depends suppdm QNAM
#' @depends suppdm QVAL
#' @depends suppdm_vaccine STUDYID
#' @depends suppdm_vaccine USUBJID
#' @depends suppdm_vaccine QNAM
#' @depends suppdm_vaccine QVAL
#' @outputs EFFICACY
#' @outputs SAFETY
#' @returns `ADSL`
supp_dm_01 <- function(ADSL, suppdm, suppdm_vaccine) {
  # Collect supplementary data
  data_supp <- rbind(suppdm, suppdm_vaccine) |>
    dplyr::select(STUDYID, USUBJID, QNAM, QVAL) |>
    dplyr::filter(QNAM %in% c("EFFICACY", "SAFETY"))

  # Transpose and join supplementary data
  tDatasetSupp <- tidyr::pivot_wider(
    data_supp,
    id_cols = c("STUDYID", "USUBJID"),
    values_from = "QVAL",
    names_from = "QNAM"
  )
  ADSL <- dplyr::left_join(ADSL, tDatasetSupp, by = c("USUBJID", "STUDYID"))
  return(ADSL)
}
