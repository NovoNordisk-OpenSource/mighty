#' @title New lbtest 04
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @code
new_lbtest <- ADLB |>
  dplyr::filter(LBTEST == "Macrocytes (new)") |>
  dplyr::mutate(LBTEST = "Macrocytes (new 2)")

if (nrow(new_lbtest) == 0) {
}

ADLB <- rbind(ADLB, new_lbtest)
