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
    data_connection = "pharmaverse",
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT
  expect_equal(actual$data_model$type,
               c("col_mutate", rep("col_echo", 4),rep("col_compute", 2), "preprocess_domain"))
  expect_equal(actual$data_model$code_id,
               c(rep(NA, 5), "arm_group_01", "arm_match_01", NA))


  actual$edges |> as.data.frame() |>  expect_snapshot_value(style = "json2")
})
