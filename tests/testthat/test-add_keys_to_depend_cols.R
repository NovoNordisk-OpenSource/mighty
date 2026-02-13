test_that("Throws error when external domain not found in trial metadata", {
  # SETUP -------------------------------------------------------------------
  trial_path <- withr::local_tempdir()
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")

  # UNKNOWN_DOMAIN is not in trial metadata YAML
  yaml_content <- "
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID, STUDYID]
population:
  base:
    - domain: dm
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

  adam_specifications <- create_temp_yaml(yaml_content)

  # ACT & EXPECT ------------------------------------------------------------

  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_trial_metadata = path_trial_metadata,
      path_trial = trial_path,
      check_cross_domain_adam_dependencies = TRUE
    ),
    "Domain 'UNKNOWN_DOMAIN' not recognized for foreign key lookup\\."
  )
})
