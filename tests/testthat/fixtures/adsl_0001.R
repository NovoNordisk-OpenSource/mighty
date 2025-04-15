#!-!
# type: derivation
# depend_cols:
#   - PLANNED_ARM
# outputs:
#   - ARM_GRP1
#!-!
arm_group_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_GRP1 = ifelse(
      PLANNED_ARM %in% c("Placebo", "Screen Failure"),
      "Placebo or Screen Failure",
      PLANNED_ARM
    ))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - ARM_GRP1
# outputs:
#   - ARM_CAT1
#!-!
arm_category_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_CAT1 = ifelse(
      ARM_GRP1 ==  "Placebo or Screen Failure",
      "Other",
      "Xanomeline"
    ))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - PLANNED_ARM
#   - ACTARM
# outputs:
#   - ARM_MATCH
#!-!
arm_match_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_MATCH = ifelse(
      PLANNED_ARM ==  ACTARM, "Match", "Mismatch"
    ))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - AGE
# outputs:
#   - AGE2
#!-!
age_crop_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(age2 = ifelse(AGE>80, 80, AGE))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - AGE
#   - AGE2
# outputs:
#   - AGE_DIFF1
#!-!
age_diff_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF1 = AGE-age2)
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - AGE_DIFF1
#   - PLANNED_ARM
# outputs:
#   - AGE_DIFF2
#!-!
age_diff_02 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - AGE2
# outputs:
#   - AGE_GRP1
#!-!
age_group_01 <- function(.self, cut_points) {
  .self <- .self |>
    dplyr::mutate(AGE_GRP1 = cut(
      age2,
      breaks = c(-Inf, cut_points, Inf),
      labels = 1:(length(cut_points) + 1),
      right = FALSE
    ))

  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - RACE
#   - COUNTRY
# outputs:
#   - RACE_COUNTRY
#!-!
race_x_country_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(RACE_COUNTRY = paste0(RACE, "-", COUNTRY))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - PLANNED_ARM
#   - USUBJID
# outputs:
#   - NEW_ARM
#!-!
arm_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(NEW_ARM = ifelse(
      USUBJID == "01-701-1015", NA, PLANNED_ARM)
    )
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - USUBJID
#   - ADLB.USUBJID
#   - ADLB.AVAL
# outputs:
#   - MIN_AVAL
#!-!
min_aval_01 <- function(.self, adlb) {
  # Extract the minimum AVAL value for each SUBJID
  .self <- .self |>
    dplyr::left_join(
      adlb |> dplyr::select(USUBJID, AVAL) |>
        dplyr::group_by(USUBJID) |>
        dplyr::summarise(MIN_AVAL = min(AVAL)) |>
        dplyr::ungroup()
      ,
      by = "USUBJID"
    )
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - DTHFL
#   - PLANNED_ARM
#   - AGE_DIFF1
# outputs:
#   - NEWFL01
#   - NEWREA01
#!-!
newfl_01 <- function(.self) {
  # Extract the minimum AVAL value for each SUBJID
  .self <- .self |> dplyr::mutate(
    NEWFL01 = ifelse(
      DTHFL == 1 & PLANNED_ARM == "Placebo" & AGE_DIFF1 > 5,
      1, 0
    )) |>
    dplyr::mutate(NEWREA01 = ifelse(NEWFL01 == 1, "Yes", "No"))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - USUBJID
#   - COUNTRY
#   - arm_match
#   - ADLB.USUBJID
#   - ADLB.LBTEST
# outputs:
#   - NEWFL02
#!-!
newfl_02 <- function(.self, ADLB) {
  subids <- ADLB |>
    dplyr::filter(LBTEST == "Polychromasia") |>
    dplyr::pull(USUBJID) |>
    unique()
  .self <- .self |>
    dplyr::mutate(NEWFL02 = ifelse(!is.na(COUNTRY) &
                                     arm_match == "MATCH" &
                                     USUBJID %in% subids, 1, 0))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - NEWFL01
#   - NEWFL02
# outputs:
#   - NEWFL03
#   - NEWREA03
#!-!
newfl_03 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(NEWFL03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , 1, 0),
                  NEWREA03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , "Yes", "No"))

  return(.self)
}

