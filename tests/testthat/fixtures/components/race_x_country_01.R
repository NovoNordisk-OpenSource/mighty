#' @title Race x country 01
#' @description A description
#' @type derivation
#' @depends ADSL RACE
#' @depends ADSL COUNTRY
#' @outputs RACE_COUNTRY
#' @returns `ADSL`
race_x_country_01 <-   function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(RACE_COUNTRY = paste0(RACE, "-", COUNTRY))
  return(ADSL)
}
