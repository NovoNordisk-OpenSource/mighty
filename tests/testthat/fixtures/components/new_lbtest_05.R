#' @title New lbtest 05
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @code
new_lbtest1 <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes (new)") |>
  dplyr::mutate(LBTEST = "Microcytes (new 2)")

new_lbtest2 <- ADLB |>
  dplyr::filter(LBTEST == "Macrocytes (new)") |>
  dplyr::mutate(LBTEST = "Macrocytes (new 2)")

if (nrow(new_lbtest1) == 0 || nrow(new_lbtest2) == 0) {
}

ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)
