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
  browser()

  x <- list.files(output_path, full.names = TRUE)

  programs <- lapply(x, readLines)
  names(programs) <- basename(x)
  # Check ADSL (program 1)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-ARM_GROUP_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-NEW_ARM-ARM_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_MATCH-ARM_MATCH_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1-ARM_GROUP_01", "ADSL-ARM_CAT1-ARM_CATEGORY_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE2-AGE_CROP_01", "ADSL-AGE_DIFF1-AGE_DIFF_01", programs[["1_ADSL.R"]])
  expect_section_order(c("ADSL-PLANNED_ARM", "ADSL-AGE_DIFF1-AGE_DIFF_01"), "ADSL-AGE_DIFF2-AGE_DIFF_02", programs[["1_ADSL.R"]])
  expect_section_order(c("ADSL-PLANNED_ARM", "ADSL-AGE_DIFF1-AGE_DIFF_01"), "ADSL-NEWFL01-NEWREA01-NEWFL_01", programs[["1_ADSL.R"]])

  source(x[[1]])
  expect_snapshot_value(x = ADSL, style = "json2")
  # Check ADLB (program 2)

  expect_section_order(c("ADLB-AVAL", "ADLB-LBTEST-NEW_LBTEST_01"), "ADLB-AVAL_GRP-AVAL_GRP_01", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-AVAL-NEW_AVAL_01", "ADLB-LBTEST-LBTEST_01", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-LBTEST-NEW_LBTEST_01", "ADLB-AVAL_GRP-AVAL_GRP_01"), "ADLB-AVAL_GRP2-AVAL_GRP_02", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-AVAL", "ADLB-AVAL-NEW_AVAL_01", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-AVAL", "ADLB-AVAL-NEW_AVAL_03", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-AVAL", "ADLB-AVALC"), "ADLB-AVAL-NEW_AVAL_04", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-LBTEST-LBTEST_01", "ADLB-LBTEST-NEW_LBTEST_01", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-AVALC", "ADLB-LBTEST-LBTEST_01"), "ADLB-LBTEST-AVALC-NEW_LBTEST_02", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-LBTEST-AVALC-NEW_LBTEST_02", "ADLB-LBTEST-NEW_LBTEST_04" , programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-LBTEST-NEW_LBTEST_01", "ADLB-LBTEST-AVALC-NEW_LBTEST_02"), "ADLB-LBTEST-NEW_LBTEST_05", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-LBTEST-LBTEST_01", "ADLB-LBTEST-NEW_LBTEST_01"), "ADLB-LBTEST-NEW_LBTEST_03", programs[["2_ADLB.R"]])
  expect_section_order(c("ADLB-AVAL", "ADLB-AVAL-NEW_AVAL_01"), "ADLB-AVAL-NEW_AVAL_02", programs[["2_ADLB.R"]])
  expect_section_order("ADLB-VISITNUM-NEW_VISITNUM_01", "ADLB-VISITNUM-NEW_VISITNUM_02", programs[["2_ADLB.R"]])

  source(x[[2]])
  expect_snapshot_value(x = ADLB, style = "json2")


  # Check ADSL (program 3)
  expect_section_order("ADSL-NEWFL02-NEWFL_02", "ADSL-NEWFL03-NEWREA03-NEWFL_03", programs[["3_ADSL.R"]])
  source(x[[3]])
  expect_snapshot_value(x = ADSL, style = "json2")

})

