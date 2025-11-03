#' @title Arm category 01
#' @description A description
#' @type derivation
#' @depends ADSL ARM_GRP1
#' @outputs ARM_CAT1
#' @code
ADSL <- ADSL |>
  dplyr::mutate(
    ARM_CAT1 = ifelse(
      ARM_GRP1 == "Placebo or Screen Failure",
      "Other",
      "Xanomeline"
    )
  )
