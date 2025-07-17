#' Age diff 02
#' 
#' @type col_compute
#' @depends ADSL AGE_DIFF1
#' @depends ADSL PLANNED_ARM
#' @outputs AGE_DIFF2
#' @returns `ADSL`
age_diff_02 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(ADSL)
}
