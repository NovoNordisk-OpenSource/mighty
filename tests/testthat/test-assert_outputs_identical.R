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
      id: {{ady_custom}}
"

  tmp_file <- withr::local_tempdir() |>
    file.path("ady.mustache")
  "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type derivation
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(A=USUBJID) |>
  dplyr::mutate(B=USUBJID)

 " |>
    writeLines(con = tmp_file)
  trial_path <- withr::local_tempdir()

  yaml_content <- whisker::whisker.render(
    yml,
    data = list(ady_custom = tmp_file)
  )
  mighty_yml_content <- "keys: {}"
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content,
    "_mighty" = mighty_yml_content
  ))

  output_path <- trial_path
  # ACT & ASSERT ------------------------------------------------------------
  generate_adam_code(
    adam_specifications = adam_specifications,
    path_trial = trial_path,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_error("Expected column outputs:")
})
