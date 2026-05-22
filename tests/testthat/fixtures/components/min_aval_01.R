#' @title Min aval 01
#' @description A description
#' @type column
#' @depends ADSL USUBJID
#' @depends ADLB USUBJID
#' @depends ADLB AVAL
#' @outputs MIN_AVAL
#' @code
# Extract the minimum AVAL value for each SUBJID
ADSL <- ADSL |>
  dplyr::left_join(
    ADLB |>
      dplyr::select(USUBJID, AVAL) |>
      dplyr::group_by(USUBJID) |>
      dplyr::summarise(MIN_AVAL = min(AVAL)) |>
      dplyr::ungroup(),
    by = "USUBJID"
  )
