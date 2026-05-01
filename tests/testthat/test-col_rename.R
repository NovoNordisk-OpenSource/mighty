
test_that("col_rename uses dplyr::rename for base domain source in ADLB", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adlb" = "col_rename_adlb_01.yml")
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = "lb"
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  # EXPECT ------------------------------------------------------------------

  ps <- actual$program_sequence

  # SRCSEQ (method: LBSEQ) where LBSEQ is not a col_copy -> col_rename
  expect_equal(ps[ps$node_id == "ADLB-SRCSEQ", ][["type"]], "col_rename")

  # Generated code uses dplyr::rename
  program_code <- actual$programs[[1]]
  expect_match(program_code, "dplyr::rename")
  expect_match(program_code, "SRCSEQ = LBSEQ")

  # init_domain selects LBSEQ (the source column)
  init_outputs <- ps[ps$node_id == "ADLB-init_domain", ]$outputs[[1]]
  expect_true("LBSEQ" %in% init_outputs)

  # write_data contains the renamed column, not the source column name
  write_code <- ps[ps$node_id == "ADLB-1-write_data", ]$code[[1]]
  expect_match(write_code, "SRCSEQ")
  expect_no_match(write_code, "\\bLBSEQ\\b")
})

test_that("col_rename falls back to col_mutate when source is a col_copy", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "skeleton_adsl_with_age_sex.yml",
      "adlb" = "categorization_of_action_type_adlb_01.yml",
      "_mighty" = "_mighty.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "lb", "sv")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = "mighty.standards",
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )
  # EXPECT ------------------------------------------------------------------

  ps <- actual$program_sequence

  init_domain_code <- ps[ps$node_id == "ADLB-init_domain", ]$code[[1]]
  expect_no_match(init_domain_code, "\\bAVAL\\b")

  # AVAL (method: LBSTRESN) where LBSTRESN IS a col_copy -> col_mutate
  expect_equal(ps[ps$node_id == "ADLB-AVAL", ][["type"]], "col_mutate")

  # AVAL2 (method: AVAL) where AVAL is a declared output column -> col_mutate
  expect_equal(ps[ps$node_id == "ADLB-AVAL2", ][["type"]], "col_mutate")
})
