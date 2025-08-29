test_that("Complex test with multiple domains and column/row operations", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- c(
    test_path("fixtures", "parameters_adlb.yml")
  )
  path_ui_data_rendered <- setup_yml_file_for_testing(
    path_ui_data,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  )

  # EXPECT ------------------------------------------------------------------

  write_adam_programs(
    dir = path_trial,
    programs = actual$programs,
    style = TRUE
  )
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check number of programs
  expect_equal(length(programs), 1)

  x[[1]] |> source()

  # Check resulting ADLB data set
  expect_setequal(
    names(ADLB),
    c(
      "USUBJID",
      "LBTEST",
      "LBSTRESN",
      "LBSTNRHI",
      "LBSTNRLO",
      "PARAM_VAR1",
      "VAR2",
      "LBSTRESN_COPY",
      "LBSTNRLO_COPY",
      "PARAM_VAR2",
      "VAR3",
      "VAR4",
      "VAR5",
      "VAR6",
      "VAR7",
      "LBSTRESN_CROP",
      "AVAL",
      "AVAL_GRP"
    )
  )

  # Check number of rows
  expect_equal(nrow(ADLB), 5404)

  # Check values of new columns
  expect_equal(ADLB[["LBSTNRLO"]], ADLB[["LBSTNRLO_COPY"]])
  expect_equal(ADLB[["LBSTRESN"]], ADLB[["LBSTRESN_COPY"]])
  expect_equal(unique(ADLB[["PARAM_VAR1"]]), "ABC")
  expect_equal(ADLB[["PARAM_VAR2"]], ADLB[["LBSTNRLO"]]+ADLB[["LBSTNRHI"]])
  expect_equal(unique(ADLB[["VAR4"]]), "A")
  expect_equal(unique(ADLB[["VAR5"]]), "B")
  expect_equal(ADLB[["VAR6"]], ADLB[["LBSTNRLO"]])
  expect_equal(ADLB[["VAR7"]], ADLB[["LBSTNRHI"]])
  expect_equal(ADLB[["AVAL"]], ADLB[["LBSTRESN"]])

})
