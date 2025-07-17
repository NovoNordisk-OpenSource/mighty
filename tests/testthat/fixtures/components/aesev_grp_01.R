
#' aesev grouping 01
#' 
#' @type col_compute
#' @depends ADAE AESEV
#' @outputs AESEV_GRP
#' @returns `ADAE`
aesev_grp_01 <-   function(ADAE) {
  ADAE <- ADAE |>
    dplyr::mutate(AESEV_GRP = ifelse(
      AESEV != "MILD", "NOT MILD", "MILD")
    )
  return(ADAE)
}
