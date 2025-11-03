#' @title aesev grouping 01
#' @description A description
#' @type derivation
#' @depends ADAE AESEV
#' @outputs AESEV_GRP
#' @code
ADAE <- ADAE |>
  dplyr::mutate(
    AESEV_GRP = ifelse(
      AESEV != "MILD",
      "NOT MILD",
      "MILD"
    )
  )
