#!-!
# type: derivation
# depend_cols:
#   - self.PLANNED_ARM
# outputs:
#   - self.ARM_GRP1
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
#   - self.ARM_GRP1
# outputs:
#   - self.ARM_CAT1
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
#   - self.PLANNED_ARM
#   - self.ACTARM
# outputs:
#   - self.arm_match
#!-!
arm_match_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(arm_match = ifelse(
      PLANNED_ARM ==  ACTARM, "Match", "Mismatch"
    ))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.AGE
# outputs:
#   - self.age2
#!-!
age_crop_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(age2 = ifelse(AGE>80, 80, AGE))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.AGE
#   - self.age2
# outputs:
#   - self.AGE_DIFF1
#!-!
age_diff_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF1 = AGE-age2)
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.AGE_DIFF1
#   - self.PLANNED_ARM
# outputs:
#   - self.AGE_DIFF2
#!-!
age_diff_02 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.age2
# outputs:
#   - self.AGE_GRP1
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
#   - self.RACE
#   - self.SITEID
# outputs:
#   - self.RACE_SITEID
#!-!
race_x_siteid_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(RACE_SITEID = paste0(RACE, "-", SITEID))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.PLANNED_ARM
#   - self.USUBJID
# outputs:
#   - self.NEW_ARM
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
#   - self.USUBJID
#   - adlb.USUBJID
#   - adlb.AVAL
# outputs:
#   - self.MIN_AVAL
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
#   - self.DTHFL
#   - self.PLANNED_ARM
#   - self.AGE_DIFF1
# outputs:
#   - self.NEWFL01
#   - self.NEWREA01
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
#   - self.USUBJID
#   - self.COUNTRY
#   - self.arm_match
#   - adlb.USUBJID
#   - adlb.LBTEST
# outputs:
#   - self.NEWFL02
#!-!
newfl_02 <- function(.self, adlb) {
  subids <- lb |>
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
#   - self.NEWFL01
#   - self.NEWFL02
# outputs:
#   - self.NEWFL03
#   - self.NEWREA03
#!-!
newfl_03 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(NEWFL03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , 1, 0),
                  NEWREA03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , "Yes", "No"))

  return(.self)
}

