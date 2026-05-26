#' @title visitnum_02
#' @description A description
#' @type column
#' @depends ADVS VISITNUM
#' @outputs VISITNUM2
#' @code
ADVS <- ADVS |>
  dplyr::mutate(
    VISITNUM2 = VISITNUM + 1
  )
