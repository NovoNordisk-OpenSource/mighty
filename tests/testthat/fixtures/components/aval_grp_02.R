#' @title Aval grp 02
#' @description A description
#' @type derivation
#' @depends ADLB AVAL_GRP
#' @outputs AVAL_GRP2
#' @code
ADLB <- ADLB |>
  dplyr::mutate(
    AVAL_GRP2 = ifelse(
      AVAL_GRP %in% c("low", "medium"),
      "low/medium",
      "high"
    )
  )
