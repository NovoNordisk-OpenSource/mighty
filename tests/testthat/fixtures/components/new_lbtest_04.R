#' @title New lbtest 04
#' @description A description 
#' @type row
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
#' @code
new_lbtest_04 <- function(ADLB) {

  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_lbtest)
  return(ADLB)
}

