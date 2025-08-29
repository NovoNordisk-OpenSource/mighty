#' @title lbtest 07
#' @description A description 
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
new_lbtest_07 <- function(ADLB, params = list(test_val = "Phosphate")) {
  if (!is.null("Protein")) {
    new_lbtest <- ADLB |>
      dplyr::filter(LBTEST == "Protein") |>
      dplyr::mutate(LBTEST = paste0("Protein", "_new"))
  } else {
    new_lbtest <- data.table()
  }

  ADLB <- rbind(ADLB, new_lbtest)
  return(ADLB)
}
