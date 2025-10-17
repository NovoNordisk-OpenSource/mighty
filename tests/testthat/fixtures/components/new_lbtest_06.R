#' @title lbtest 06
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @depends ADLB AVAL
#' @depends ADLB DOMAIN
#' @outputs LBTEST
#' @returns `ADLB`
#' @code
new_lbtest_06 <- function(ADLB) {
  new_lbtest1 <- ADLB |>
    dplyr::filter(LBTEST == "Microcytes (new)" & AVAL > 1 & !is.na(DOMAIN)) |>
    dplyr::mutate(LBTEST = "Microcytes (new 3)")

  new_lbtest2 <- ADLB |>
    dplyr::filter(LBTEST == "Microcytes (new 2)" & AVAL > 1 & !is.na(DOMAIN)) |>
    dplyr::mutate(LBTEST = "Microcytes (new 4)")

  if (nrow(new_lbtest1) == 0 || nrow(new_lbtest2) == 0) {
  }

  ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)
  return(ADLB)
}
