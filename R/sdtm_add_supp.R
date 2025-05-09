#' Add supp data - our version of NNsdtm
#' @description
#' This verison allows adding without a merging ID - but only for domains with 1 record per subject
#'
#' @param data_main
#' @param data_supp
#' @param dataset
#'
#' @return
#' @export
#'
#' @examples
sdtm_add_supp <- function (data_main, data_supp, dataset = "dm")
{
  suppId <- paste0("supp", dataset)
  oldlabels_datasetMain <- unlist(labelled::var_label(data_main))
  supp_labels <- data_supp |> dplyr::distinct(.data$QNAM, .data$QLABEL)
  tDatasetSupp <- tidyr::pivot_wider(
    data_supp,
    id_cols = c("STUDYID", "USUBJID", "IDVARVAL"),
    values_from = "QVAL",
    names_from = "QNAM"
  )

  # Some domains (dm and ?) might sometimes be merged without the ID var.
  # MEWP: Verify with trials when and why these cases exist - ideally all should
  # use the IDVARVAL from the Supp
  no_idvarval <- isTRUE(all(tDatasetSupp$IDVARVAL == ""))
  if (no_idvarval) {
    tmp <- tDatasetSupp |> dplyr::select(!IDVARVAL)
    dataset <- dplyr::left_join(data_main, tmp, by = c("USUBJID", "STUDYID"))
  } else {
    dsIdVar <- names(data_main)[grep(pattern = "*SPID", names(data_main))]
    names(tDatasetSupp)[names(tDatasetSupp) == "IDVARVAL"] <- dsIdVar
    dataset <- merge(
      x = data_main,
      y = tDatasetSupp,
      by = intersect(names(data_main), names(tDatasetSupp)),
      all.x = TRUE,
      suffixes = c("")
    )
  }
  new_label <- c(oldlabels_datasetMain,
                 structure(supp_labels$QLABEL, names = supp_labels$QNAM))
  names_new <- intersect(colnames(dataset), names(new_label))
  labelled::var_label(dataset)[names_new] <- new_label[names_new]
  return(dataset)
}
