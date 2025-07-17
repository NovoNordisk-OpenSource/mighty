#' New lbtest 05
#' 
#' @type row_compute
#' @depends ADLB LBTEST
#' @outputs LBTEST
#' @returns `ADLB`
new_lbtest_05 <- function(ADLB) {

  new_lbtest1 <- ADLB |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  new_lbtest2 <-   ADLB |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
    # stop("No rows to add.")
  }

  ADLB <-   rbind(ADLB, new_lbtest1, new_lbtest2)
  return(ADLB)
}

