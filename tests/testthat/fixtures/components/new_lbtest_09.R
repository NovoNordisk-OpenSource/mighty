#' @title New lbtest 09
#' @description A description 
#' @type row_compute
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
new_lbtest_09 <- function(ADLB) {
  if (!is.null("Phosphate")) {
    new_lbtest <- ADLB |>
      dplyr::filter(LBTEST == "Phosphate") |>
      dplyr::mutate(LBTEST = paste0("Phosphate", "_new"))
  } else {
    new_lbtest <- data.table()
  }

  ADLB <- rbind(ADLB, new_lbtest)
  return(ADLB)
}
