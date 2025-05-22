#' arm_group_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - PLANNED_ARM
#' outputs:
#'   - ARM_GRP1
#' type: col_compute
#' ```
#'
arm_group_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_GRP1 = ifelse(
      PLANNED_ARM %in% c("Placebo", "Screen Failure"),
      "Placebo or Screen Failure",
      PLANNED_ARM
    ))
  return(.self)
}

#' arm_category_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - ARM_GRP1
#' outputs:
#'   - ARM_CAT1
#' type: col_compute
#' ```
#'
arm_category_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_CAT1 = ifelse(
      ARM_GRP1 ==  "Placebo or Screen Failure",
      "Other",
      "Xanomeline"
    ))
  return(.self)
}

#' arm_match_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - PLANNED_ARM
#'   - ACTARM
#' outputs:
#'   - ARM_MATCH
#' type: col_compute
#' ```
#'
arm_match_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_MATCH = ifelse(
      PLANNED_ARM ==  ACTARM, "Match", "Mismatch"
    ))
  return(.self)
}

#' age_crop_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - AGE
#' outputs:
#'   - AGE2
#' type: col_compute
#' ```
#'
age_crop_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE2 = ifelse(AGE>80, 80, AGE))
  return(.self)
}

#' age_diff_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - AGE
#'   - AGE2
#' outputs:
#'   - AGE_DIFF1
#' type: col_compute
#' ```
#'
age_diff_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF1 = AGE-AGE2)
  return(.self)
}

#' age_diff_02
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - AGE_DIFF1
#'   - PLANNED_ARM
#' outputs:
#'   - AGE_DIFF2
#' type: col_compute
#' ```
#'
age_diff_02 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(.self)
}

#' age_group_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - AGE2
#' outputs:
#'   - AGE_GRP1
#' type: col_compute
#' ```
#'
age_group_01 <-  function(.self, params = list(cut_points = c(25, 50))) {
  .self <- .self |>
    dplyr::mutate(AGE_GRP1 = cut(
      AGE2,
      breaks = c(-Inf, params$cut_points, Inf),
      labels = 1:(length(params$cut_points) + 1),
      right = FALSE
    ))

  return(.self)
}

#' race_x_country_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - RACE
#'   - COUNTRY
#' outputs:
#'   - RACE_COUNTRY
#' type: col_compute
#' ```
#'
race_x_country_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(RACE_COUNTRY = paste0(RACE, "-", COUNTRY))
  return(.self)
}

#' arm_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - PLANNED_ARM
#'   - USUBJID
#' outputs:
#'   - NEW_ARM
#' type: col_compute
#' ```
#'
arm_01 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(NEW_ARM = ifelse(
      USUBJID == "01-701-1015", NA, PLANNED_ARM)
    )
  return(.self)
}

#' min_aval_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#'   - ADLB.USUBJID
#'   - ADLB.AVAL
#' outputs:
#'   - MIN_AVAL
#' type: col_compute
#' ```
#'
min_aval_01 <-  function(.self, ADLB) {
  # Extract the minimum AVAL value for each SUBJID
  .self <- .self |>
    dplyr::left_join(
      ADLB |> dplyr::select(USUBJID, AVAL) |>
        dplyr::group_by(USUBJID) |>
        dplyr::summarise(MIN_AVAL = min(AVAL)) |>
        dplyr::ungroup()
      ,
      by = "USUBJID"
    )
  return(.self)
}

#' newfl_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - DTHFL
#'   - PLANNED_ARM
#'   - AGE_DIFF1
#' outputs:
#'   - NEWFL01
#'   - NEWREA01
#' type: col_compute
#' ```
#'
newfl_01 <-  function(.self) {
  # Extract the minimum AVAL value for each SUBJID
  .self <- .self |> dplyr::mutate(
    NEWFL01 = ifelse(
      DTHFL == 1 & PLANNED_ARM == "Placebo" & AGE_DIFF1 > 5,
      1, 0
    )) |>
    dplyr::mutate(NEWREA01 = ifelse(NEWFL01 == 1, "Yes", "No"))
  return(.self)
}

#' newfl_02
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#'   - COUNTRY
#'   - ARM_MATCH
#'   - ADLB.USUBJID
#'   - ADLB.LBTEST
#' outputs:
#'   - NEWFL02
#' type: col_compute
#' ```
#'
newfl_02 <-  function(.self, ADLB) {
  subids <- ADLB |>
    dplyr::filter(LBTEST == "Polychromasia") |>
    dplyr::pull(USUBJID) |>
    unique()
  .self <-  .self |>
    dplyr::mutate(NEWFL02 = ifelse(!is.na(COUNTRY) &
                                     ARM_MATCH == "MATCH" &
                                     USUBJID %in% subids, 1, 0))
  return(.self)
}

#' newfl_03
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - NEWFL01
#'   - NEWFL02
#' outputs:
#'   - NEWFL03
#'   - NEWREA03
#' type: col_compute
#' ```
#'
newfl_03 <-  function(.self) {
  .self <- .self |>
    dplyr::mutate(NEWFL03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , 1, 0),
                  NEWREA03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , "Yes", "No"))

  return(.self)
}

#' age_redefined_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - core.AGE
#'   - core.SEX
#'   - RACE
#' outputs:
#'   - AGE
#' type: col_compute
#' ```
#'
age_redefined_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE = ifelse(!is.na(SEX) & !is.na(RACE), AGE, 0))
  return(.self)
}

#' age_redefined_02
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - core.AGE
#' outputs:
#'   - AGE
#' type: col_compute
#' ```
#'
age_redefined_02 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE = AGE + 1)
  return(.self)
}

#' age2_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - core.AGE
#' outputs:
#'   - AGE2
#' type: col_compute
#' ```
#'
age2_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE2 = 10*AGE)
  return(.self)
}

#' age3_01
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - AGE
#' outputs:
#'   - AGE3
#' type: col_compute
#' ```
#'
age3_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE3 = AGE - 1)
  return(.self)
}
