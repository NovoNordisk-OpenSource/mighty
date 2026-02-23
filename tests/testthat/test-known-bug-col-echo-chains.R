test_that("Known bug when doing chains of col_echos using same names", {
  # SETUP -------------------------------------------------------------------

  yaml_content_adsl <- "
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

  - id: STUDYID2
    method: ADLB.STUDYID2 

  - id: STUDYID3
    method: ADLB.STUDYID3
"

  yaml_content_adlb <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID, STUDYID]
population:
  base:
    - domain: lb
      depends:
        - VISITNUM
        - LBTESTCD
      filter: \"(VISITNUM == 13 & LBTESTCD == 'BASO')\"
  global:
    - filter: NA
      depends:
        - NA
columns:
  - id: USUBJID

  - id: STUDYID

  - id: VISITNUM

  - id: LBTESTCD

  - id: STUDYID2
    method: STUDYID

  - id: STUDYID3
    method: ADSL.STUDYID2 

"
  mighty_yml_content <- "keys: {}"
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content_adsl,
    "adlb" = yaml_content_adlb,
    "_mighty" = mighty_yml_content
  ))

  path_trial <- withr::local_tempdir()

  standards_lib <- "mighty.standards"

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "lb")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  )

  #remove connector save step from program 3 as this will fail on Windows
  actual$programs[3][[1]] <-
    remove_connector_write_step(actual$programs[3][[1]])

  write_adam_programs(actual$programs, path_trial, style = TRUE)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)

  # ASSERT -----------------------------------------------------------------

  expect_no_error(source(x[[1]]))
  expect_no_error(source(x[[2]]))
  expect_no_error(source(x[[3]]))
  expect_contains(names(ADSL), "STUDYID2")

  # This error occurs because program 4 (ADLB prog 2) contains STUDYID2 variable
  expect_error(source(x[[4]]), "Column `STUDYID2` doesn't exist")
  # not currently supported by mighty.
})
