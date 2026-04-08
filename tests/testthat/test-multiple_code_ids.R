test_that("Multiple columns using same code_id, but diff parameters errors out", {
  skip(
    message = "This check has not been re-implemented after switching to mighty.component"
  )
  # SETUP
  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adlb" = "multiple_code_ids.yml",
      "_mighty" = "_mighty.yml"
    ),
    process_glue = FALSE
  )
  std_lib_path <- c(testthat::test_path("fixtures", "multiple_code_ids.R"))
  output_path <- withr::local_tempdir()

  # ACT
  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = get_connector_config_path(output_path)
    ),
    regexp = "Code_id `fn_AB` is used in multiple columns with different parameters"
  )
})
