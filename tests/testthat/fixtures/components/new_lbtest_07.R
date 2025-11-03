#' @title lbtest 07
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @code
if (!is.null("Protein")) {
  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Protein") |>
    dplyr::mutate(LBTEST = paste0("Protein", "_new"))
} else {
  new_lbtest <- data.table()
}

ADLB <- rbind(ADLB, new_lbtest)
