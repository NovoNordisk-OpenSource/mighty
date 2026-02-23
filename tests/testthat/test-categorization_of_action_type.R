test_that("Check that action types are classified correctly", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "skeleton_adsl_with_age_sex.yml",
      "adlb" = "categorization_of_action_type_adlb_01.yml",
      "_mighty" = "_mighty.yml"
    )
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb", "sv")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  ps <- actual$program_sequence
  expect_equal(ps[ps$node_id == "ADLB-2-read_data", ][["type"]], "read_data")
  expect_equal(ps[ps$node_id == "ADLB-init_domain", ][["type"]], "init_domain")
  expect_equal(ps[ps$node_id == "ADLB-PLANNED_ARM", ][["type"]], "col_echo")
  expect_equal(ps[ps$node_id == "ADLB-ARM", ][["type"]], "col_echo")
  expect_equal(ps[ps$node_id == "ADLB-AVAL", ][["type"]], "col_mutate")
  expect_equal(ps[ps$node_id == "ADLB-AVAL_GRP", ][["type"]], "col_compute")
  expect_equal(ps[ps$node_id == "ADLB-AVAL2", ][["type"]], "col_mutate")
  expect_equal(ps[ps$node_id == "ADLB-LBTEST", ][["type"]], "col_compute")
  expect_equal(
    ps[ps$node_id == "ADLB-AVALFL-AVALREA", ][["type"]],
    "col_compute"
  )
  expect_equal(
    ps[ps$node_id == "ADLB-new_microcytes", ][["type"]],
    "row_compute"
  )
  expect_equal(ps[ps$node_id == "ADLB-2-write_data", ][["type"]], "write_data")
})
