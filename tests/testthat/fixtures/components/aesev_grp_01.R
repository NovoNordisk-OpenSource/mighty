#' @title aesev grouping 01
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
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
