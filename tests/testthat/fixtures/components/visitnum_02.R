#' @title visitnum_02
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADVS VISITNUM
#' @outputs VISITNUM2
#' @code
ADVS <- ADVS |>
  dplyr::mutate(
    VISITNUM2 = VISITNUM + 1
  )
