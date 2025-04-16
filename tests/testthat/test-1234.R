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


  # Check ADLB (program 2)
  browser()
  expect_section_order(c("ADLB-AVAL", "ADLB-LBTEST-NEW_LBTEST_01"), "ADLB-AVAL_GRP-AVAL_GRP_01", programs[["2_ADLB.R"]])
  expect_section_order("ROW_01", "DER_11", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-LBTEST-NEW_LBTEST_01", "ADLB-AVAL_GRP-AVAL_GRP_01"), "DER_26", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-AVAL", "ROW_01", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-AVAL", "ROW_03", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-AVAL", "PRED_04"), "ROW_04", programs[["2_ADLB.R"]])
  expect_section_order("DER_11", "ADLB-LBTEST-NEW_LBTEST_01", programs[["2_ADLB.R"]])
  expect_section_order(c("PRED_04", "DER_11"), "ROW_06", programs[["2_ADLB.R"]])
  expect_section_order("ROW_06", "ADLB-LBTEST-new_lbtest_04" , programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-LBTEST-NEW_LBTEST_01", "ROW_06"), "ROW_08", programs[["2_ADLB.R"]])
  expect_section_order(c("DER_11", "ADLB-LBTEST-NEW_LBTEST_01"), "ROW_09", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-AVAL", "ROW_01"), "ROW_10", programs[["2_ADLB.R"]])
  expect_section_order("ROW_02", "ROW_11", programs[["2_ADLB.R"]])
})
waldo::compare(head(ADSL), b)
