#' Lbtest2
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self LBTEST
#' @outputs LBTEST2
#' @returns `.self`
lbtest2 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(
      LBTEST2 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      ))
  return(.self)
}

#' Lbtest3
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self LBTEST
#' @outputs LBTEST3
#' @outputs LBTEST3_FLG
#' @returns `.self`
lbtest3 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(
      LBTEST3 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      )) |>
    dplyr::mutate(
      LBTEST3_FLG = dplyr::case_when(
        LBTEST3 == "Invalid" ~ "Y",
        LBTEST3 != "Invalid" ~ "Invalid"
      ))
  return(.self)
}
