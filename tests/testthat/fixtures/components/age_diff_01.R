
#' Age diff 01
#' 
#' @type col_compute
#' @depends ADSL AGE
#' @depends ADSL AGE2
#' @outputs AGE_DIFF1
#' @returns `ADSL`
age_diff_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE_DIFF1 = AGE-AGE2)
  return(ADSL)
}
