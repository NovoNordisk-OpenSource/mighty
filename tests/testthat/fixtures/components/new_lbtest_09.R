#' @title New lbtest 09
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @code
if (!is.null("Phosphate")) {
  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Phosphate") |>
    dplyr::mutate(LBTEST = paste0("Phosphate", "_new"))
} else {
  new_lbtest <- data.table()
}

ADLB <- rbind(ADLB, new_lbtest)
