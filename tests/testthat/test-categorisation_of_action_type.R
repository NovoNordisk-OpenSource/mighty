test_that("Check that action types are classified correctly", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- testthat::test_path("fixtures", "categorisation_of_action_type_adlb_01.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb", "sv")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  ps <- actual$program_sequence
  expect_equal(ps[ps$node_id == "ADLB-1-read_data", ][["type"]], "read_data")
  expect_equal(ps[ps$node_id == "ADLB-init_domain", ][["type"]], "init_domain")
  expect_equal(ps[grepl("ADLB-PLANNED_ARM-", ps$node_id), ][["type"]], "col_echo")
  expect_equal(ps[grepl("ADLB-ARM-", ps$node_id), ][["type"]], "col_echo")
  expect_equal(ps[grepl("ADLB-AVAL-", ps$node_id), ][["type"]], "col_mutate")
  expect_equal(ps[grepl("ADLB-AVAL_GRP-", ps$node_id), ][["type"]], "col_compute")
  expect_equal(ps[grepl("ADLB-AVAL2-", ps$node_id), ][["type"]], "col_mutate")
  expect_equal(ps[grepl("ADLB-LBTEST-.*\\/lbtest_01\\.R$", ps$node_id), ][["type"]], "col_compute")
  expect_equal(ps[grepl("ADLB-AVALFL-AVALREA-", ps$node_id), ][["type"]], "col_compute")
  expect_equal(ps[grepl("ADLB-LBTEST-.*\\/new_lbtest_01\\.R$", ps$node_id), ][["type"]], "row_compute")
  expect_equal(ps[ps$node_id == "ADLB-1-write_data", ][["type"]], "write_data")

})



