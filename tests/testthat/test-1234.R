test_that("perfomance", {
  # SETUP
  ui_path <- c(
    test_path("fixtures", "adsl_1234.yml"),
    test_path("fixtures", "adlb_1234.yml")
  )
  path_trial_metadata <- test_path("fixtures", "init_0001.yml")
  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT

  actual <- generate_adam_code(ui_path,
                               std_lib_path,
                               path_trial_metadata,
                               domain_keys_path,
                               output_path,
                               data_connection = "pharmaverse")
  write_adam_programs(dir = output_path, programs = actual$programs)

  x <- list.files(output_path, full.names = TRUE)
  do.call(file.edit, as.list(x))
browser()
  programs <- lapply(x, readLines)
  names(programs) <- basename(x)
  # Check ADSL (program 1)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-NEW_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_MATCH", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1", "ADSL-ARM_CAT1", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE2", "ADSL-AGE_DIFF1", programs[["1_ADSL.R"]])
  expect_section_order(c("ADSL-PLANNED_ARM", "ADSL-AGE_DIFF1"), "ADSL-AGE_DIFF2", programs[["1_ADSL.R"]])
  expect_section_order(c("ADSL-PLANNED_ARM", "ADSL-AGE_DIFF1"), "ADSL-NEWFL01-NEWREA01", programs[["1_ADSL.R"]])
})
