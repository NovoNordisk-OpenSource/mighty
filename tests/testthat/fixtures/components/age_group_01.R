#' @title Age group 01
#' @description A description 
#' @type derivation
#' @depends ADSL AGE2
#' @outputs AGE_GRP1
#' @returns `ADSL`
#' @code
age_group_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(AGE_GRP1 = cut(
      AGE2,
      breaks = c(-Inf, c(20, 45), Inf),
      labels = 1:(length(c(20, 45)) + 1),
      right = FALSE
    ))

  return(ADSL)
}
