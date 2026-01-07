test_that("Bug in cross-adam dependencies", {
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
    method: STUDYID
"
  yaml_file_adsl <- create_temp_yaml(yaml_content_adsl)
  yaml_file_adlb <- create_temp_yaml(yaml_content_adlb)

  adam_specifications <- c(yaml_file_adsl, yaml_file_adlb)

  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_0001.yml"
  )
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
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  )

  # ASSERT -----------------------------------------------------------------
  #remove connector save step from program 3 as this will fail on Windows
  actual$programs[3][[1]] <-
    remove_connector_write_step(actual$programs[3][[1]])

  write_adam_programs(actual$programs, path_trial, style = TRUE)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)

  expect_no_error(source(x[[1]]))
  expect_no_error(source(x[[2]]))
  expect_no_error(source(x[[3]]))
  expect_contains(names(ADSL), "STUDYID2")
})
