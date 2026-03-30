#' @title New lbtest 01
#' @description A description
#' @type row
#' @depends ADLB LBTEST
#' @depends SV VISITNUM
#' @depends SV USUBJID
#' @depends SV VISITDY
#' @outputs LBTEST
#' @code
new_lbtest <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes") |>
  dplyr::mutate(LBTEST = "Microcytes (new)")

new_lbtest <- new_lbtest |>
  dplyr::left_join(SV, by = c("USUBJID", "VISITNUM")) |>
  dplyr::filter(VISITDY > 20) |>
  dplyr::select(-VISITDY)

ADLB <- rbind(ADLB, new_lbtest)
