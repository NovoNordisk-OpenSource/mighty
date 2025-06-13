test_that("Supplementary data is added right after read_data when no supp cols are used in filters", {

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adsl_01.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
debugonce(replace_core_with_named_domain)
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

  # EXPECT ------------------------------------------------------------------

  browser()
  do.call(file.edit, as.list(x))

})

test_that("Supplementary data is added right after read_data when a supp col is used in global filter", {

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adsl_02.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------

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

  # EXPECT ------------------------------------------------------------------


  browser()
  do.call(file.edit, as.list(x))

})

test_that("Supplementary data is added right after read_data when a supp col is used in domain filter", {

  skip()

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adsl_03.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------

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

  # EXPECT ------------------------------------------------------------------

  browser()
  do.call(file.edit, as.list(x))

})

test_that("Supplementary data is added right after read_data when supp cols and ADSL cols are used in filters", {


  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adae_01.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
# debugonce(generate_initialize_domain)
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

  # EXPECT ------------------------------------------------------------------

  browser()
  do.call(file.edit, as.list(x))

})
