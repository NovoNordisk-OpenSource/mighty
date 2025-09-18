test_that("Multiple columns using same code_id, but diff parameters errors out", {
  skip(message = "This check has not been re-implimented after switching to mighty.component")
  # SETUP
  path_ui_data <- testthat::test_path(
    "fixtures",
    "multiple_code_ids.yml"
  )
  path_ui_data_rendered <- setup_yml_file_for_testing(
    path_ui_data,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(testthat::test_path("fixtures", "multiple_code_ids.R"))
  output_path <- withr::local_tempdir()

  # ACT
  expect_error(
    generate_adam_code(
      path_ui_data = ui_path,
      path_trial_metadata = path_trial_metadata,
      path_output = output_path
    ),
    regexp = "Code_id `fn_AB` is used in multiple columns with different paramenters"
  )
})
