#' @title Lbtest 01
#' @description A description 
#' @type derivation
#' @depends ADLB USUBJID
#' @depends ADLB LBSEQ
#' @depends ADLB AVAL
#' @depends lb USUBJID
#' @depends lb LBSEQ
#' @depends lb LBTEST
#' @depends ADSL USUBJID
#' @depends ADSL PLANNED_ARM
#' @outputs LBTEST
#' @returns `ADLB`
#' @code
lbtest_01 <-   function(ADLB, lb, ADSL) {

  # join with adsl, only derive lbtest if adsl.planned_arm != "" and
  # if aval is na them lbtest = "Invalid"
  ADLB <- ADLB |>
    dplyr::left_join(
      lb |> dplyr::select(USUBJID, LBSEQ, LBTEST),
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

  return(ADLB)
}
