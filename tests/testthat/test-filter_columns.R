test_that("Global filter and domain filter are equivalent when same filter use on each domain", {
  # ARRANGE -----------------------------------------------------------------
  path_trial <- withr::local_tempdir()
  mighty_yml <- "keys:
  dm:
    - USUBJID
  dm_vaccine:
    - USUBJID"

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )

  # YAML 1 -----------------------------------------------------------------
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
"

  adam_specifications <- setup_study_dir(list(
    "adsl" = yml_1,
    "_mighty" = mighty_yml
  ))

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
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
  yml_2 <- "
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
"

  adam_specifications <- setup_study_dir(list(
    "adlb" = yml_2,
    "_mighty" = mighty_yml
  ))

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
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
