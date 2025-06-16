#' supp_ae_01
#' @param .self `data.frame` Input data set
#' @type col_supp
#' @depends core STUDYID
#' @depends core USUBJID
#' @depends suppae STUDYID
#' @depends suppae USUBJID
#' @depends suppae QNAM
#' @depends suppae QLABEL
#' @depends suppae QVAL
#' @outputs AETRTEM
#' @returns `.self`
supp_ae_01 <- function(.self, suppae) {
  # Collect supplementary data
  data_supp <- suppae |>
    dplyr::filter(QNAM == "AETRTEM")

  # Transpose and join supplementary data
  supp_labels <- data_supp |> dplyr::distinct(.data$QNAM, .data$QLABEL)
  tDatasetSupp <- tidyr::pivot_wider(
    data_supp,
    id_cols = c("STUDYID", "USUBJID"),
    values_from = "QVAL",
    names_from = "QNAM"
  )
  .self <- dplyr::left_join(.self, tDatasetSupp, by = c("USUBJID", "STUDYID"))
  return(.self)
}
