test_that("Throws error when external domain not found in _mighty.yml", {
  # SETUP -------------------------------------------------------------------
  trial_path <- withr::local_tempdir()

  # UNKNOWN_DOMAIN is not in _mighty.yml
  yaml_content <- "
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID, STUDYID]
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

  - id: STUDYID

  - id: UNKNOWN_VAR
    method: UNKNOWN_DOMAIN.SOME_COLUMN
"

  mighty_yml_content <- "study_id: test_study"

  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content,
    "_mighty" = mighty_yml_content
  ))

  # ACT & EXPECT ------------------------------------------------------------

  error_msg <- expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = trial_path,
      check_cross_domain_adam_dependencies = TRUE
    )
  )

  error_text <- get_error_text(error_msg)
  expect_match(error_text, "Specification validation errors")
  expect_match(error_text, "Unrecognized domain")
  expect_match(error_text, "UNKNOWN_DOMAIN")
  expect_match(error_text, "not recognized for foreign key lookup")
})
