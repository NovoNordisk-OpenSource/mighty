#' @title Min aval 01
#' @description A description 
#' @type col_compute
#' @depends ADSL USUBJID
#' @depends ADLB USUBJID
#' @depends ADLB AVAL
#' @outputs MIN_AVAL
#' @returns `ADSL`
min_aval_01 <-   function(ADSL, ADLB) {
  # Extract the minimum AVAL value for each SUBJID
  ADSL <- ADSL |>
    dplyr::left_join(
      ADLB |> dplyr::select(USUBJID, AVAL) |>
        dplyr::group_by(USUBJID) |>
        dplyr::summarise(MIN_AVAL = min(AVAL)) |>
        dplyr::ungroup()
      ,
      by = "USUBJID"
    )
  return(ADSL)
}
