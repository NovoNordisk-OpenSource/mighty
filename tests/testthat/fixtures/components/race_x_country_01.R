#' @title Race x country 01
#' @description A description
#' @type derivation
#' @depends ADSL RACE
#' @depends ADSL COUNTRY
#' @outputs RACE_COUNTRY
#' @code
ADSL <- ADSL |>
  dplyr::mutate(RACE_COUNTRY = paste0(RACE, "-", COUNTRY))
