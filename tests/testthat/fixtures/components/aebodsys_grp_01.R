#' @title aebodsys grouping 01
#' @description A description 
#' @type derivation
#' @depends ADAE AEBODSYS
#' @depends ADAE AESEV_GRP
#' @outputs AEBODSYS_GRP
#' @returns `ADAE`
aebodsys_grp_01 <-   function(ADAE) {
  ADAE <- ADAE |>
    dplyr::mutate(AEBODSYS_GRP = ifelse(
      grepl("DISORDER", AEBODSYS, ignore.case = TRUE) & AESEV_GRP == "MILD",
      "MILD DISORDER", "OTHER")
    )
  return(ADAE)
}
