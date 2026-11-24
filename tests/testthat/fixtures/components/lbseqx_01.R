#' @title Lbseqx 01
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADLB LBSEQ
#' @outputs LBSEQX
#' @code
ADLB <- ADLB |>
  dplyr::mutate(LBSEQX = LBSEQ * 10)
