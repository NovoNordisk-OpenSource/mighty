test_that("Complex test with multiple domains and column/row operations", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adlb" = "parameters_adlb.yml", "_mighty" = "_mighty.yml")
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  # EXPECT ------------------------------------------------------------------

  write_adam_programs(
    dir = path_connector_config,
    programs = actual$programs,
    style = TRUE
  )
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # Check number of programs
  expect_equal(length(programs), 1)

  x[[1]] |> source()

  # Check resulting ADLB data set
  expect_setequal(
    names(ADLB),
    c(
      "USUBJID",
      "STUDYID",
      "LBSEQ",
      "LBTEST",
      "LBSTRESN",
      "LBSTNRHI",
      "LBSTNRLO",
      "PARAM_VAR1",
      "VAR2",
      "LBSTRESN_COPY",
      "LBSTNRLO_COPY",
      "PARAM_VAR2",
      "VAR3",
      "VAR4",
      "VAR5",
      "VAR6",
      "VAR7",
      "LBSTRESN_CROP",
      "AVAL",
      "AVAL_GRP"
    )
  )

  # Check number of rows
  expect_equal(nrow(ADLB), 5404)

  # Check values of new columns
  expect_equal(ADLB[["LBSTNRLO"]], ADLB[["LBSTNRLO_COPY"]])
  expect_equal(ADLB[["LBSTRESN"]], ADLB[["LBSTRESN_COPY"]])
  expect_equal(unique(ADLB[["PARAM_VAR1"]]), "ABC")
  expect_equal(ADLB[["PARAM_VAR2"]], ADLB[["LBSTNRLO"]] + ADLB[["LBSTNRHI"]])
  expect_equal(unique(ADLB[["VAR4"]]), "A")
  expect_equal(unique(ADLB[["VAR5"]]), "B")
  expect_equal(ADLB[["VAR6"]], ADLB[["LBSTNRLO"]])
  expect_equal(ADLB[["VAR7"]], ADLB[["LBSTNRHI"]])
  expect_equal(ADLB[["AVAL"]], ADLB[["LBSTRESN"]])
})
