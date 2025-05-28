test_that(
  "Error is triggered for a col_copy and col_echo action having the same output",
  {
    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_outputs_adsl_01.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(
      regexp = "Column\\(s\\) ARM are outputted in multiple actions in domain ADSL.")
  }
)

test_that(
  "Error is triggered for a col_copy and col_compute action having an output in common",
  {

    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_outputs_adsl_02.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(
      regexp = "Column\\(s\\) AGE are outputted in multiple actions in domain ADSL.")

  }
)

test_that(
  "Error is triggered for a col_copy and col_echo action having the same output in two domains",
  {
  skip(message = "Not implemented yet.")
  }
)


test_that(
  "Error is triggered for a col_compute action with invalid column name in UI data",
  {
    skip(message = "Not implemented yet.")

  }
)


test_that(
  "Check how an action with two outputs where only one is added as an action is handled",
  {
    skip(message = "Not implemented yet.")
  }
)



