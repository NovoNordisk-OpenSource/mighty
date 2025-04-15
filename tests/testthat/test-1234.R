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
  expect_section_order("PRED_01", "DER_02", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "PRED_02"), "DER_03", programs[["1_adsl.R"]])
  expect_section_order("PRED_01", "DER_05", programs[["1_adsl.R"]])
  expect_section_order("DER_02", "DER_06", programs[["1_adsl.R"]])
  expect_section_order("DER_01", "DER_08", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "DER_08"), "DER_09", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "DER_08"), "DER_27", programs[["1_adsl.R"]])


})
