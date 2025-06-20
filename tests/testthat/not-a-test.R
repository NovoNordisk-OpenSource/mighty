test_that("Complex test with multiple domains and column/row operations", {
  # This is not a test, but is used to demo the project
  ui_path <- c(
    test_path("fixtures", "adsl_complex.yml"),
    test_path("fixtures", "adlb_complex.yml")
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )
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
  programs <- x |> lapply(readLines)

  vis_code_tree(nodes = actual$data_for_visualization, edges = actual$edges)
  names(programs) <- basename(x)

})
