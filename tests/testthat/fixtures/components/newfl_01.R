#' @title Newfl 01
#' @description A description
#' @type column
#' @depends ADSL DTHFL
#' @depends ADSL PLANNED_ARM
#' @depends ADSL AGE_DIFF1
#' @outputs NEWFL01
#' @outputs NEWREA01
#' @code
# Extract the minimum AVAL value for each SUBJID
ADSL <- ADSL |>
  dplyr::mutate(
    NEWFL01 = ifelse(
      DTHFL == 1 & PLANNED_ARM == "Placebo" & AGE_DIFF1 > 5,
      1,
      0
    )
  ) |>
  dplyr::mutate(NEWREA01 = ifelse(NEWFL01 == 1, "Yes", "No"))
