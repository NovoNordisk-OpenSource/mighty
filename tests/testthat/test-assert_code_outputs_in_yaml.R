test_that("Error when code component outputs not referenced in YML", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
population:
  base:
    - domain: DM
      depends:
        - NA
      filter: NA
  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID
  - id: A
    component:
      id: {{{ady_custom}}}
"

  tmp_file <- withr::local_tempdir() |>
    file.path("ady.mustache")
  "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(A=USUBJID) |>
  dplyr::mutate(B=USUBJID)

 " |>
    writeLines(con = tmp_file)

  yaml_content <- whisker::whisker.render(
    yml,
    data = list(ady_custom = tmp_file)
  )
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})

test_that("Error when code component output has no overlap with YML", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
population:
  base:
    - domain: DM
      depends:
        - NA
      filter: NA
  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID
  - id: A
    component:
      id: {{{ady_custom}}}
"

  tmp_file <- withr::local_tempdir() |>
    file.path("ady.mustache")
  "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @depends ADSL USUBJID
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(B=USUBJID)

 " |>
    writeLines(con = tmp_file)

  yaml_content <- whisker::whisker.render(
    yml,
    data = list(ady_custom = tmp_file)
  )
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})
