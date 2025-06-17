test_that("Multiple columns using same code_id, but diff parameters errors out", {
  # SETUP
  ui_path <- c(test_path("fixtures", "multiple_code_ids.yml"))
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(testthat::test_path("fixtures", "multiple_code_ids.R"))

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  expect_error(
    generate_adam_code(
      ui_path,
      std_lib_path,
      path_trial_metadata,
      domain_keys_path,
      output_path
    ),
    regexp = "Code_id `fn_AB` is used in multiple columns with different paramenters"
  )
})
