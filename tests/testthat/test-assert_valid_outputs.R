test_that("Error for col_compute action w/ invalid column name in UI data", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adsl" = "assert_valid_outputs_adsl_03.yml", "_mighty" = "_mighty.yml")
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      adam_specifications = adam_specifications,
      standards_lib = standards_lib,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    "Expected column outputs:\"AGE\" Actual column outputs:\"AGE_INVALID\" Missing from expected outputs:AGE",
    error_msg_clean
  ))
})


test_that("Error is triggered for a col_compute with two outputs that only have one output specified in the UI data", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adlb" = "assert_valid_outputs_adlb_01.yml", "_mighty" = "_mighty.yml")
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      adam_specifications = adam_specifications,
      standards_lib = standards_lib,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    "Expected column outputs:\"AVALFL\",\"AVALREA\" Actual column outputs:\"AVALFL\" Missing from expected outputs:AVALREA", # nolint: line_length_linter
    error_msg_clean
  ))
})
