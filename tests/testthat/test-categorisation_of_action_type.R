test_that("Check that predecessors and derivations are identified correctly and that predecessors can be renamed while extracting from core", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "adsl_categorisation_of_action_type.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

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
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT
  expect_equal(actual$data_model$type,
               c("mutate", rep("echo", 2),rep("compute", 2), "domain_init"))
  expect_equal(actual$data_model$code_id,
               c(NA, NA, NA, "arm_group_01", "arm_match_01", NA))

  x[[1]] |> source()
  ADSL |> expect_snapshot_value(style = "json2")
})
