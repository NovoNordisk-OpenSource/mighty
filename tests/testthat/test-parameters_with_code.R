test_that("parameters accept R code", {
  # SETUP
  ui_path <- c(test_path("fixtures", "parameters_with_code.yml"))
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(testthat::test_path("fixtures", "parameters_with_code.R"))

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT

  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )

  # EXPECT

  eval(parse(text = paste0(actual$programs$`1_ADSL`, collapse = "\n")))
  expect_equal(
    unname(ADSL),
    list(
      100,
      "This is a regular string",
      1,
      NULL,
      6,
      5,
      "User-supplied string"
    )
  )

  # Ensure that default arguments are passed as un-evaluated R strings to the program
  prog <- actual$programs$`1_ADSL` |> unlist()
  expect_equal(1, grepl("param_5 = min\\(6, 7\\)", prog) |> sum())
})
