#' @title Newfl 02
#' @description A description
#' @type derivation
#' @depends ADSL USUBJID
#' @depends ADSL COUNTRY
#' @depends ADSL ARM_MATCH
#' @depends ADLB USUBJID
#' @depends ADLB LBTEST
#' @outputs NEWFL02
#' @returns `ADSL`
#' @code
newfl_02 <- function(ADSL, ADLB) {
  subids <- ADLB |>
    dplyr::filter(LBTEST == "Polychromasia") |>
    dplyr::pull(USUBJID) |>
    unique()
  ADSL <- ADSL |>
    dplyr::mutate(
      NEWFL02 = ifelse(
        !is.na(COUNTRY) &
          ARM_MATCH == "MATCH" &
          USUBJID %in% subids,
        1,
        0
      )
    )
  return(ADSL)
}
