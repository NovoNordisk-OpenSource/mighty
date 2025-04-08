test_that("Complex test", {

  # SETUP ----------------------------------------------------------------------
  ui_path <- c(
    test_path("fixtures", "init_0001.yml"),
    test_path("fixtures", "adsl_0001.yml"),
    test_path("fixtures", "adlb_0001.yml")
  )

  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ------------------------------------------------------------------------

  actual <- make_adam_program(ui_path,
                              std_lib_path,
                              domain_keys_path,
                              output_path,
                              data_connection = "pharmaverse")

  x <- list.files(output_path, full.names = TRUE)
  programs <- lapply(x, readLines)
  names(programs) <- basename(x)

  # EXPECT data values ---------------------------------------------------------

  # Execute code and run expectations on the final outputed ADaM tables
  lapply(x, source)
  expect_snapshot_value(adsl, style = "json2")
  expect_snapshot_value(adlb, style = "json2")

  # EXPECT programs -----------------------------------------------------------

  # These are very sensitive expectations that will fail when any character is
  # changed in the program. They are mostly useful to highlight what changes
  # happen when refactoring/adding features
  expect_snapshot_value(programs$`1_adsl.R`, style = "json2")
  expect_snapshot_value(programs$`2_adlb.R`, style = "json2")
  expect_snapshot_value(programs$`3_adlb.R`, style = "json2")

  # EXPECT Topology ------------------------------------------------------------

  # These tests ensure that the parts of the topology that cannot change, do not
  # change. Some parts of the topology can change without any meaningful impact
  # (e.g. some predecessors that are executed later could be moved forward
  # without changing the outcome)

  # Check ADSL (program 1)
  expect_section_order("PRED_01", "DER_02", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "PRED_02"), "DER_03", programs[["1_adsl.R"]])
  expect_section_order("PRED_01", "DER_05", programs[["1_adsl.R"]])
  expect_section_order("DER_02", "DER_06", programs[["1_adsl.R"]])
  expect_section_order("DER_01", "DER_08", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "DER_08"), "DER_09", programs[["1_adsl.R"]])
  expect_section_order(c("PRED_01", "DER_08"), "DER_27", programs[["1_adsl.R"]])

  # Check ADLB (program 2)
  expect_section_order("ROW_07", "DER_24", programs[["2_adlb.R"]])
  expect_section_order(c("PRED_03", "ROW_05"), "DER_25", programs[["2_adlb.R"]])
  expect_section_order("ROW_01", "DER_11", programs[["2_adlb.R"]])
  expect_section_order(c("ROW_05", "DER_25"), "DER_26", programs[["2_adlb.R"]])
  expect_section_order("PRED_03", "ROW_01", programs[["2_adlb.R"]])
  expect_section_order("PRED_03", "ROW_03", programs[["2_adlb.R"]])
  expect_section_order(c("PRED_03", "PRED_04"), "ROW_04", programs[["2_adlb.R"]])
  expect_section_order("DER_11", "ROW_05", programs[["2_adlb.R"]])
  expect_section_order(c("PRED_04", "DER_11"), "ROW_06", programs[["2_adlb.R"]])
  expect_section_order("ROW_06", "ROW_07", programs[["2_adlb.R"]])
  expect_section_order(c("ROW_05", "ROW_06"), "ROW_08", programs[["2_adlb.R"]])
  expect_section_order(c("DER_11", "ROW_05"), "ROW_09", programs[["2_adlb.R"]])
  expect_section_order(c("PRED_03", "ROW_01"), "ROW_10", programs[["2_adlb.R"]])
  expect_section_order("ROW_02", "ROW_11", programs[["2_adlb.R"]])

  # Check ADSL (program 3)
  expect_section_order("DER_20", "DER_07", programs[["3_adsl.R"]])

  # Storing the data.table in the snapshot causes warnings, maybe due to
  # internal attributes use by data.table. So we convert to data.frame
  expect_snapshot_value(as.data.frame(actual$program_sequence[, .(domain, node_id, program_id, rank)]), style = "json2")
})
