#' @title Arm category 01
#' @description A description 
#' @type col_compute
#' @depends ADSL ARM_GRP1
#' @outputs ARM_CAT1
#' @returns `ADSL`
arm_category_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(ARM_CAT1 = ifelse(
      ARM_GRP1 ==  "Placebo or Screen Failure",
      "Other",
      "Xanomeline"
    ))
  return(ADSL)
}
