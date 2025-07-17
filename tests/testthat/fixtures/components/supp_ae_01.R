#' supp_ae_01
#'
#' @type col_compute
#' @depends ADAE STUDYID
#' @depends ADAE USUBJID
#' @depends suppae STUDYID
#' @depends suppae USUBJID
#' @depends suppae QNAM
#' @depends suppae QVAL
#' @outputs AETRTEM
#' @returns `.self`
supp_ae_01 <- function(ADAE, suppae) {
  # Collect supplementary data
  data_supp <- suppae |>
    dplyr::select(STUDYID, USUBJID, QNAM, QVAL) |>
    dplyr::filter(QNAM == "AETRTEM")

  # Transpose and join supplementary data
  tDatasetSupp <- tidyr::pivot_wider(
    data_supp,
    id_cols = c("STUDYID", "USUBJID"),
    values_from = "QVAL",
    names_from = "QNAM"
  )
  ADAE <- dplyr::left_join(ADAE, tDatasetSupp, by = c("USUBJID", "STUDYID"))
  return(ADAE)
}
