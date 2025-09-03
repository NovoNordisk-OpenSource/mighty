test_that("Error is triggered for a col_compute action with invalid column name in UI data", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adsl_03.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl("Expected column outputs:\"AGE\" Actual column outputs:\"AGE_INVALID\" Missing from expected outputs:AGE",
                    error_msg_clean))
})


test_that("Error is triggered for a col_compute with two outputs that only have one output specified in the UI data", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adlb_01.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
    )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl("Expected column outputs:\"AVALFL\",\"AVALREA\" Actual column outputs:\"AVALFL\" Missing from expected outputs:AVALREA",
                    error_msg_clean))

})

