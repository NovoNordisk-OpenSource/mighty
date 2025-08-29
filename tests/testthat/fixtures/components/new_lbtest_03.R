#' @title New lbtest 03
#' @description A description 
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
new_lbtest_03 <- function(ADLB) {

  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_lbtest)
  return(ADLB)
}

