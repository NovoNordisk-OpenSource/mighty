#' Dummy component creating a Albumin grouping variable
#' @description
#' Based on the baseline readings (`ABLFL == "Y"`) for Albumin
#' (`PARAMCD == "ALB"`) a new grouping variable (`ALBBLGRP`) is
#' added with the following intervals:
#'
#' * Albumin levels at least 40 g/L: ">= 40 g/L"
#' * Albumin levels below 40 g/L: "< 40 g/L"
#' * Missing or no baseline levels: "Missing"
#'
#' @param .self `data.frame` Input
#' @param adlb `data.frame` ADLB data set
#' @type derivation
#' @depends .self USUBJID
#' @depends adlb USUBJID
#' @depends adlb PARAMCD
#' @depends adlb AVAL
#' @outputs ALBBLGRP
#' @returns `.self` with added subject level Albumin grouping at baseline
#' @examples
#' pharmaverseadam::adsl |>
#'   dplyr::select(USUBJID) |>
#'   dummy(adlb = pharmaverseadam::adlb)
#'
#' @export
dummy <- function(.self, adlb) {
  x <- adlb |>
    dplyr::filter(.data$PARAMCD == "ALB", .data$ABLFL == "Y") |>
    dplyr::transmute(
      .data$USUBJID,
      ALBBLGRP = dplyr::case_when(
        .data$AVAL >= 40 ~ ">= 40 g/L",
        !is.na(.data$AVAL) ~ "< 40 g/L",
        TRUE ~ "Missing"
      )
    )

  .self <- .self |>
    dplyr::left_join(x, by = "USUBJID") |>
    dplyr::mutate(ALBBLGRP = dplyr::coalesce(.data$ALBBLGRP, "Missing"))

  return(.self)
}
