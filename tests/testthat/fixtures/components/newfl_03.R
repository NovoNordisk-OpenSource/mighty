#' @title Newfl 03
#' @description A description
#' @type derivation
#' @depends ADSL NEWFL01
#' @depends ADSL NEWFL02
#' @outputs NEWFL03
#' @outputs NEWREA03
#' @returns `ADSL`
#' @code
newfl_03 <- function(ADSL) {
  ADSL <- ADSL |>
    dplyr::mutate(
      NEWFL03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1, 1, 0),
      NEWREA03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1, "Yes", "No")
    )

  return(ADSL)
}
