test_that("Known bug when a data_context is used with a domain split over two programs", {
  # SETUP -------------------------------------------------------------------

  # ADSL needs ADLB.STUDYID2 and ADLB is built from LB, so ADSL is split into
  # program 1 (built from DM) and program 3 (reads ADSL back to add STUDYID2).
  # Program 3 has no mighty_init_domain action.
  yaml_content_adsl <- "
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
    - domain: LB
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
    method: ADLB.STUDYID
"
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content_adsl,
    "adlb" = yaml_content_adlb
  ))

  path_connector_config <- withr::local_tempdir()

  # All SDTM data that the specification needs is available.
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "lb")
  )
  cnt <- connector::connect(get_connector_config_path(path_connector_config))

  # ACT & ASSERT ------------------------------------------------------------

  # The write step of ADSL program 3 looks up the mighty_init_domain action of
  # its program. There is none, so the lookup gives a row of NA and
  # can_execute becomes NA. `if (!action$can_execute)` then fails.
  # See handle_write_domain_action() in R/add_check_executable_status.R.
  #
  # When fixed, replace with expect_no_error() and check that
  # all(actual$executable_program_sequence$can_execute) is TRUE.
  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = get_connector_config_path(path_connector_config),
      check_cross_domain_adam_dependencies = TRUE,
      data_context = data_context$new(cnt)
    ),
    "missing value where TRUE/FALSE needed"
  )
})
