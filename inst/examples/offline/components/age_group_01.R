#' @title Age group 01
#' @description Grouping of age
#' @type column
#' @origin Derived
#' @method Age cut into the groups < 18, 18-64, 65-74 and >= 75 years.
#' @depends ADSL AGE
#' @outputs AGE_GRP1
#' @returns `ADSL`
#' @code
ADSL <- ADSL |>
  dplyr::mutate(
    AGE_GRP1 = cut(
      AGE,
      breaks = c(-Inf, 18, 65, 75, Inf),
      labels = c("< 18 years", "18 - 64 years", "65 - 74 years", ">= 75 years"),
      right = FALSE
    )
  )
