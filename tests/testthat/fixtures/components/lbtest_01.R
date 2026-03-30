#' @title Lbtest 01
#' @description A description
#' @type derivation
#' @depends ADLB USUBJID
#' @depends ADLB LBSEQ
#' @depends ADLB AVAL
#' @depends LB USUBJID
#' @depends LB LBSEQ
#' @depends LB LBTEST
#' @depends ADSL USUBJID
#' @depends ADSL PLANNED_ARM
#' @outputs LBTEST
#' @code
# Join with ADSL, only derive LBTEST if ADSL.PLANNED_ARM != "" and
# if AVAL is NA then LBTEST = "Invalid"
ADLB <- ADLB |>
  dplyr::left_join(
    LB |> dplyr::select(USUBJID, LBSEQ, LBTEST),
    by = c("USUBJID" = "USUBJID", "LBSEQ" = "LBSEQ")
  ) |>
  dplyr::left_join(
    ADSL |> dplyr::select(USUBJID, PLANNED_ARM),
    by = "USUBJID"
  ) |>
  dplyr::mutate(
    LBTEST = ifelse(
      !is.na(AVAL) & PLANNED_ARM != "",
      LBTEST,
      "Invalid"
    )
  ) |>
  dplyr::select(-PLANNED_ARM)
