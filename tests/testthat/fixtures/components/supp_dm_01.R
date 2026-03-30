#' @title supp_dm_01
#' @description A description
#' @type derivation
#' @depends ADSL STUDYID
#' @depends ADSL USUBJID
#' @depends SUPPDM STUDYID
#' @depends SUPPDM USUBJID
#' @depends SUPPDM QNAM
#' @depends SUPPDM QVAL
#' @depends SUPPDM_VACCINE STUDYID
#' @depends SUPPDM_VACCINE USUBJID
#' @depends SUPPDM_VACCINE QNAM
#' @depends SUPPDM_VACCINE QVAL
#' @outputs EFFICACY
#' @outputs SAFETY
#' @code
# Collect supplementary data
data_supp <- rbind(SUPPDM, SUPPDM_VACCINE) |>
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
