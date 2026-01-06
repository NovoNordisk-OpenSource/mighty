test_that("Global filter and domain filter are equivilent when same filter use on each domain", {
  # ARRANGE -----------------------------------------------------------------
  path_trial <- withr::local_tempdir()
  trial_yml <- '
trial_id: "1"
project_id: "01"
complete_id: "01-1"
instance: "current"

keys:
  dm:
    - USUBJID
  dm_vaccine:
    - USUBJID
'
  path_trial_metadata <- file.path(path_trial, "trial_metadata.yml")
  writeLines(text = trial_yml, con = path_trial_metadata)
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )

  # YAML 1 -----------------------------------------------------------------
  adam_specifications <- file.path(path_trial, "ui_yml.yml")
  yml_1 <- "
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID]
population:
  base:
    - domain: dm
      depends:
        - SEX
      filter: 'SEX == \"F\"'
    - domain: dm_vaccine
      depends:
        - SEX
      filter: 'SEX == \"F\"'
  global:
    - filter: NA
      depends:
        - NA
columns:
  - id: USUBJID
  - id: SEX
" |>
    writeLines(con = adam_specifications)

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(
    dir = path_trial,
    programs = actual$programs,
    style = TRUE
  )
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  adsl_1 <- source(x[[1]])

  # YAML 2 -----------------------------------------------------------------
  yml_1 <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: dm
      depends:
        - NA
      filter: NA
    - domain: dm_vaccine
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SEX == \"F\"'
      depends:
        - SEX
columns:
  - id: USUBJID
  - id: SEX
" |>
    writeLines(con = adam_specifications)

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(
    dir = path_trial,
    programs = actual$programs,
    style = TRUE
  )
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  adsl_2 <- source(x[[1]])

  # ASSERT -----------------------------------------------------------------
  expect_identical(adsl_1, adsl_2)
})
