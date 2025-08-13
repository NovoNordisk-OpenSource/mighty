#' @title New lbtest 02
#' @description A description 
#' @type row_compute
#' @depends ADLB LBTEST
#' @depends ADLB AVALC
#' @outputs LBTEST
#' @outputs AVALC
#' @returns `ADLB`
new_lbtest_02 <- function(ADLB) {

  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == "Macrocytes" &
                    as.numeric(gsub("[N<]",   "", AVALC))>0.5 &
                    !is.na(AVALC)) |>
    dplyr::mutate(LBTEST = "Macrocytes (new)",
                  AVALC = "")

  ADLB <-   rbind(ADLB, new_lbtest)
  return(ADLB)
}

