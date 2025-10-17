#' @title lbtest 07
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
#' @code
new_lbtest_08 <- function(ADLB, params = list(test_val = "Phosphate")) {
  if (!is.null("Specific Gravity")) {
    new_lbtest <- ADLB |>
      dplyr::filter(LBTEST == "Specific Gravity") |>
      dplyr::mutate(LBTEST = paste0("Specific Gravity", "_new"))
  } else {
    new_lbtest <- data.table()
  }

  ADLB <- rbind(ADLB, new_lbtest)
  return(ADLB)
}
