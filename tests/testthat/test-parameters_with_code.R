test_that("parameters accept R code", {
  # SETUP
  ui_path <- c(test_path("fixtures", "parameters_with_code.yml"))
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(testthat::test_path("fixtures", "parameters_with_code.R"))

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()
  setup_testdata(testdata = "pharmaverse", test_data_path = output_path)


  # ACT

  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path
  )
  # EXPECT
  expect_snapshot_value(actual$programs$`1_ADSL`[[3]], style = "json2")
  expect_snapshot_value(actual$programs$`1_ADSL`[[4]], style = "json2")
  expect_snapshot_value(actual$programs$`1_ADSL`[[5]], style = "json2")

  # Ensure that default arguments are passed as un-evaluated R strings to the program
  prog <- actual$programs$`1_ADSL` |> unlist()
  expect_equal(1, grepl("param_5 = min\\(6, 7\\)", prog) |> sum())
})

test_that("collect_parameters handles basic single parameter set", {
  # ARRANGE -------------------------------------------------------------------
  parameters <- list(list(param1 = "value1", param2 = "value2"))
  code_id <- "TEST_001"

  # ACT -----------------------------------------------------------------------
  result <- collect_parameters(parameters, code_id)

  # ASSERT --------------------------------------------------------------------
  # Should return a list containing the parameters as a named list
  expect_type(result, "list")
  expect_length(result, 1)
  # Parameters should be preserved with their names and values
  expect_equal(result[[1]], list(param1 = "value1", param2 = "value2"))
})


test_that("collect_parameters handles NA parameters", {
  # ARRANGE -------------------------------------------------------------------
  parameters <- list(NA_character_)
  code_id <- "TEST_002"

  # ACT -----------------------------------------------------------------------
  result <- collect_parameters(parameters, code_id)

  # ASSERT --------------------------------------------------------------------
  # Should return list with NA when no parameters are provided
  expect_type(result, "list")
  expect_length(result, 1)
  # NA parameters should be returned as NA_character_
  expect_true(is.na(result[[1]]))
})

test_that("collect_parameters handles multiple identical parameter sets", {
  # ARRANGE -------------------------------------------------------------------
  parameters <- list(
    list(param1 = "value1", param2 = "value2"),
    list(param1 = "value1", param2 = "value2")
  )
  code_id <- "TEST_003"

  # ACT -----------------------------------------------------------------------
  result <- collect_parameters(parameters, code_id)

  # ASSERT --------------------------------------------------------------------
  # Should deduplicate identical parameters and return single set
  expect_type(result, "list")
  expect_length(result, 1)
  # Duplicated parameters should be merged into single parameter set
  expect_equal(result[[1]], list(param1 = "value1", param2 = "value2"))
})


test_that("collect_parameters throws error for conflicting parameters", {
  # ARRANGE -------------------------------------------------------------------
  parameters <- list(
    list(param1 = "value1", param2 = "value2"),
    list(param1 = "different_value", param2 = "value2")
  )
  code_id <- "TEST_004"

  # ACT & ASSERT --------------------------------------------------------------
  # Should throw error when same parameter name has different values
  expect_error(
    collect_parameters(parameters, code_id),
    "Code_id `TEST_004` is used in multiple columns with different paramenters",
    # Error should occur because conflicting parameter values violate the constraint
    # that columns sharing a code_id must have identical parameterization
    fixed = TRUE
  )
})

test_that("collect_parameters handles mixed parameter names", {
  # ARRANGE -------------------------------------------------------------------
  parameters <- list(
    list(param1 = "value1", param2 = "value2"),
    list(param3 = "value3", param1 = "value1")
  )
  code_id <- "TEST_006"

  # ACT -----------------------------------------------------------------------
  result <- collect_parameters(parameters, code_id)

  # ASSERT --------------------------------------------------------------------
  # Should combine all unique parameter name-value pairs
  expect_type(result, "list")
  expect_length(result, 1)
  expected_params <- list(param1 = "value1", param2 = "value2", param3 = "value3")
  # All unique parameter combinations should be preserved in the result
  expect_equal(result[[1]], expected_params)
})
