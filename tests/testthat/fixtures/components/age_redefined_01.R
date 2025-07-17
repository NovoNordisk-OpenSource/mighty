#' age_redefined_01
#' 
#' @type col_compute
#' @depends core AGE
#' @depends core SEX
#' @depends ADLB RACE
#' @outputs AGE
#' @returns `ADLB`
age_redefined_01 <- function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AGE = ifelse(!is.na(SEX) & !is.na(RACE), AGE, 0))
  return(ADLB)
}
