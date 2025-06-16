test_that("Supplementary data is added right after read_data when no supp cols are used in filters", {

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adsl_01.yml")
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

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "ADSL-EFFICACY-SAFETY-supp_dm_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-EFFICACY-SAFETY-supp_dm_01", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM" ,programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1"))
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

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "ADSL-EFFICACY-SAFETY-supp_dm_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-EFFICACY-SAFETY-supp_dm_01", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM" ,programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 234)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1"))

})

test_that("Supplementary data is added right after read_data when a supp col is used in domain filter", {

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

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "ADSL-EFFICACY-SAFETY-supp_dm_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-EFFICACY-SAFETY-supp_dm_01", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM" ,programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1"))

})


test_that("Supplementary data is added right after read_data when supp cols and ADSL cols are used in filters", {

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adae_01.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adae_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------

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

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check program order
  expect_section_order("Read data sets", "Initialize ADAE", programs[["1_ADAE.R"]])
  expect_section_order("Initialize ADAE", "ADAE-AETRTEM-supp_ae_01", programs[["1_ADAE.R"]])
  expect_section_order("ADAE-AETRTEM-supp_ae_01", "Preprocess ADAE", programs[["1_ADAE.R"]])
  expect_section_order("Preprocess ADAE", "Save ADAE" ,programs[["1_ADAE.R"]])

})
