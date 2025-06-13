#' Arm group 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self PLANNED_ARM
#' @outputs ARM_GRP1
#' @returns `.self`
arm_group_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_GRP1 = ifelse(
      PLANNED_ARM %in% c("Placebo", "Screen Failure"),
      "Placebo or Screen Failure",
      PLANNED_ARM
    ))
  return(.self)
}

#' Arm category 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self ARM_GRP1
#' @outputs ARM_CAT1
#' @returns `.self`
arm_category_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_CAT1 = ifelse(
      ARM_GRP1 ==  "Placebo or Screen Failure",
      "Other",
      "Xanomeline"
    ))
  return(.self)
}

#' Arm match 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self PLANNED_ARM
#' @depends .self ACTARM
#' @outputs ARM_MATCH
#' @returns `.self`
arm_match_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(ARM_MATCH = ifelse(
      PLANNED_ARM ==  ACTARM, "Match", "Mismatch"
    ))
  return(.self)
}

#' Age crop 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE
#' @outputs AGE2
#' @returns `.self`
age_crop_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE2 = ifelse(AGE>80, 80, AGE))
  return(.self)
}

#' Age diff 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE
#' @depends .self AGE2
#' @outputs AGE_DIFF1
#' @returns `.self`
age_diff_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF1 = AGE-AGE2)
  return(.self)
}

#' Age diff 02
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE_DIFF1
#' @depends .self PLANNED_ARM
#' @outputs AGE_DIFF2
#' @returns `.self`
age_diff_02 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE_DIFF2 = ifelse(PLANNED_ARM != "Placebo", AGE_DIFF1, NA))
  return(.self)
}

#' Age group 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE2
#' @outputs AGE_GRP1
#' @returns `.self`
age_group_01 <-   function(.self, params = list(cut_points = c(25, 50))) {
  .self <- .self |>
    dplyr::mutate(AGE_GRP1 = cut(
      AGE2,
      breaks = c(-Inf, params$cut_points, Inf),
      labels = 1:(length(params$cut_points) + 1),
      right = FALSE
    ))

  return(.self)
}

#' Race x country 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self RACE
#' @depends .self COUNTRY
#' @outputs RACE_COUNTRY
#' @returns `.self`
race_x_country_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(RACE_COUNTRY = paste0(RACE, "-", COUNTRY))
  return(.self)
}

#' Arm 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self PLANNED_ARM
#' @depends .self USUBJID
#' @outputs NEW_ARM
#' @returns `.self`
arm_01 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(NEW_ARM = ifelse(
      USUBJID == "01-701-1015", NA, PLANNED_ARM)
    )
  return(.self)
}

#' Min aval 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self USUBJID
#' @depends ADLB USUBJID
#' @depends ADLB AVAL
#' @outputs MIN_AVAL
#' @returns `.self`
min_aval_01 <-   function(.self, ADLB) {
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

#' Newfl 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self DTHFL
#' @depends .self PLANNED_ARM
#' @depends .self AGE_DIFF1
#' @outputs NEWFL01
#' @outputs NEWREA01
#' @returns `.self`
newfl_01 <-   function(.self) {
  # Extract the minimum AVAL value for each SUBJID
  .self <- .self |> dplyr::mutate(
    NEWFL01 = ifelse(
      DTHFL == 1 & PLANNED_ARM == "Placebo" & AGE_DIFF1 > 5,
      1, 0
    )) |>
    dplyr::mutate(NEWREA01 = ifelse(NEWFL01 == 1, "Yes", "No"))
  return(.self)
}

#' Newfl 02
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self USUBJID
#' @depends .self COUNTRY
#' @depends .self ARM_MATCH
#' @depends ADLB USUBJID
#' @depends ADLB LBTEST
#' @outputs NEWFL02
#' @returns `.self`
newfl_02 <-   function(.self, ADLB) {
  subids <- ADLB |>
    dplyr::filter(LBTEST == "Polychromasia") |>
    dplyr::pull(USUBJID) |>
    unique()
  .self <-   .self |>
    dplyr::mutate(NEWFL02 = ifelse(!is.na(COUNTRY) &
                                     ARM_MATCH == "MATCH" &
                                     USUBJID %in% subids, 1, 0))
  return(.self)
}

#' Newfl 03
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self NEWFL01
#' @depends .self NEWFL02
#' @outputs NEWFL03
#' @outputs NEWREA03
#' @returns `.self`
newfl_03 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(NEWFL03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , 1, 0),
                  NEWREA03 = ifelse(NEWFL01 == 1 & NEWFL02 == 1 , "Yes", "No"))

  return(.self)
}

#' age_redefined_01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends core AGE
#' @depends core SEX
#' @depends .self RACE
#' @outputs AGE
#' @returns `.self`
age_redefined_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE = ifelse(!is.na(SEX) & !is.na(RACE), AGE, 0))
  return(.self)
}

#'#' age_redefined_02
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends core AGE
#' @outputs AGE
#' @returns `.self`
age_redefined_02 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE = AGE + 1)
  return(.self)
}

#' age2_01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends core AGE
#' @outputs AGE2
#' @returns `.self`
age2_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE2 = 10*AGE)
  return(.self)
}

#' age3_01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE
#' @outputs AGE3
#' @returns `.self`
age3_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE3 = AGE - 1)
  return(.self)
}

#' age4_01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AGE2
#' @outputs AGE4
#' @returns `.self`
age4_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE4 = AGE2 - 2)
  return(.self)
}

#' age_sex_redefined_01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends core AGE
#' @depends core SEX
#' @depends .self RACE
#' @outputs AGE
#' @outputs SEX
#' @returns `.self`
age_sex_redefined_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AGE = ifelse(!is.na(SEX) & !is.na(RACE), AGE, 0),
                  SEX = toupper(SEX))
  return(.self)
}

#' supp_dm_01
#' @param .self `data.frame` Input data set
#' @type col_supp
#' @depends core STUDYID
#' @depends core USUBJID
#' @depends suppdm STUDYID
#' @depends suppdm USUBJID
#' @depends suppdm QNAM
#' @depends suppdm QLABEL
#' @depends suppdm QVAL
#' @depends suppdm_vaccine STUDYID
#' @depends suppdm_vaccine USUBJID
#' @depends suppdm_vaccine QNAM
#' @depends suppdm_vaccine QLABEL
#' @depends suppdm_vaccine QVAL
#' @outputs EFFICACY
#' @outputs SAFETY
#' @returns `.self`
supp_dm_01 <- function(.self, suppdm, suppdm_vaccine) {

  # Collect supplementary data
  data_supp <- rbind(suppdm, suppdm_vaccine) |>
    dplyr::filter(QNAM %in% c("EFFICACY", "SAFETY"))

  # Transpose and join supplementary data
  supp_labels <- data_supp |> dplyr::distinct(.data$QNAM, .data$QLABEL)
  tDatasetSupp <- tidyr::pivot_wider(
    data_supp,
    id_cols = c("STUDYID", "USUBJID"),
    values_from = "QVAL",
    names_from = "QNAM"
  )
  .self <- dplyr::left_join(.self, tDatasetSupp, by = c("USUBJID", "STUDYID"))

  return(.self)
}
