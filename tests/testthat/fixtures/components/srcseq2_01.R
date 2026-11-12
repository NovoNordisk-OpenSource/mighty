#' @title Srcseq2 01
#' @description A description
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADLB SRCSEQ
#' @outputs SRCSEQ2
#' @code
ADLB <- ADLB |>
  dplyr::mutate(SRCSEQ2 = SRCSEQ * 2)
