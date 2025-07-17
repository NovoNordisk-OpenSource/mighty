#' Aval grp 02
#' 
#' @type col_compute
#' @depends ADLB AVAL_GRP
#' @outputs AVAL_GRP2
#' @returns `ADLB`
aval_grp_02 <-   function(ADLB) {
  ADLB <- ADLB |>
    dplyr::mutate(AVAL_GRP2 = ifelse(
      AVAL_GRP %in% c("low", "medium"), "low/medium", "high")
    )
  return(ADLB)
}


