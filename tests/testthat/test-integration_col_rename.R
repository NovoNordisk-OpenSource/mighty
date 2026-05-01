test_that("col_rename generated ADLB program executes without error", {
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
    standards_lib = "mighty.standards",
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(
    dir = path_connector_config,
    programs = actual$programs,
    style = TRUE
  )

  # EXPECT ------------------------------------------------------------------

  x <- list.files(path_connector_config, pattern = "\\.R$", full.names = TRUE)
  expect_no_error(source(x[[1]]))
})
