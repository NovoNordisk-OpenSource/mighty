#' New lbtest 01
#' 
#' @type row_compute
#' @depends ADLB LBTEST
#' @depends sv VISITNUM
#' @depends sv USUBJID
#' @depends sv VISITDY
#' @outputs LBTEST
#' @returns `ADLB`
new_lbtest_01 <-   function(ADLB) {

  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Microcytes") |>
    dplyr::mutate(LBTEST = "Microcytes (new)")

  new_lbtest <-   new_lbtest |>
    dplyr::left_join(sv, by = c("USUBJID", "VISITNUM")) |>
    dplyr::filter(VISITDY > 20) |>
    dplyr::select(-VISITDY)

  ADLB <-   rbind(ADLB, new_lbtest)
  return(ADLB)
}

