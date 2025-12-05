test_that("Multiple columns using same code_id, but diff parameters errors out", {
  skip(
    message = "This check has not been re-implimented after switching to mighty.component"
  )
  # SETUP
  adam_specifications <- testthat::test_path(
    "fixtures",
    "multiple_code_ids.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(testthat::test_path("fixtures", "multiple_code_ids.R"))
  output_path <- withr::local_tempdir()

  # ACT
  expect_error(
    generate_adam_code(
      adam_specifications = ui_path,
      path_trial_metadata = path_trial_metadata,
      path_output = output_path,
      mighty.metadata = FALSE
    ),
    regexp = "Code_id `fn_AB` is used in multiple columns with different paramenters"
  )
})
