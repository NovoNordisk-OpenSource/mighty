#' @title Age crop 01
#' @description A description 
#' @type col_compute
#' @depends ADSL AGE
#' @outputs AGE2
#' @returns `ADSL`
age_crop_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE2 = ifelse(AGE>80, 80, AGE))
  return(ADSL)
}
