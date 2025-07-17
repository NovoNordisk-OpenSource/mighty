test_that(
  "No error is triggered for a complete specification when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_01.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()

    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "No error is triggered for a complete specification when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_01.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_no_error()

    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "An error is triggered when cross-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {

    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_02.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE",
                      error_msg_clean))
  }
)


test_that(
  "No error is triggered when cross-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_02.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()

    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "An error is triggered when within-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADLB spec: ADLB.VISITNUMto execute: ADLB.VISITNUM2",
                      error_msg_clean))
  }
)


test_that(
  "An error is triggered when within-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADLB.VISITNUMto execute: ADLB.VISITNUM2",
                      error_msg_clean))
  }
)


test_that(
  "An error is triggered when within-domain and cross-domain ADaM specifications are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_04.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADLB.STUDYID ADLB.VISITNUM ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE ADLB.VISITNUM2",
                      error_msg_clean))
  }
)


test_that(
  "An error is triggered when within-domain and cross-domain ADaM specifications are incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_04.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADLB.STUDYID ADLB.VISITNUM ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE ADLB.VISITNUM2",
                      error_msg_clean))
  }
)


test_that(
  "An error is triggered when ADSL specification for filtering is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_05.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter",
                      error_msg_clean))
  }
)


test_that(
  "No error is triggered when ADSL specification for filtering is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_05.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()
    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "An error is triggered when ADSL specification for filtering and actions is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE",
                      error_msg_clean))
  }
)


test_that(
  "No error is triggered when ADSL specification for filtering and actions is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()

    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "An error is triggered when ADSL specifications for filtering in two domains are incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
    )
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE ADVS filter",
                      error_msg_clean))
  }
)

test_that(
  "No error is triggered when ADSL specifications for filtering in two domains are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP -------------------------------------------------------------------

    path_ui_data <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
    )
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # EXPECT -------------------------------------------------------------------

    actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()

    actual$data_model$actions |>
      names() |>
      sort() |>
      expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "An error is triggered when within-domain ADaM specifications for two domains are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    path_ui_data <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_02.yml")
    )
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADLB spec: ADLB.VISITNUMto execute: ADLB.VISITNUM2.*ADVS spec: ADVS.VISITNUMto execute: ADVS.VISITNUM2",
                      error_msg_clean))
  }
)

test_that(
  "An error is triggered when the within-domain ADaM specification that includes a standard component is incomplete and check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    path_ui_data <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_07.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".* ADLB spec: ADLB.LBTESTto execute: ADLB.LBTEST2 ADLB.LBTEST3 ADLB.LBTEST3_FLG",
                      error_msg_clean))
  }
)


test_that(
  "An error is triggered when the within-domain ADaM specification that includes a standard component is incomplete and check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    path_ui_data <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_07.yml")
    path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    path_trial <- withr::local_tempdir()

    setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("lb")
    )
    standards_lib <- "mighy.standards"

    # ACT -------------------------------------------------------------------

    error_msg <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

    # EXPECT -------------------------------------------------------------------

    error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
    expect_true(grepl(".*ADaM spec: ADLB.LBTESTto execute: ADLB.LBTEST2 ADLB.LBTEST3 ADLB.LBTEST3_FLG",
                      error_msg_clean))

  }
)


test_that("An error is triggered when filter_depend_cols is incomplete with check_cross_domain_adam_dependencies enabled", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_08.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(".*ADaM spec: ADLB.USUBJID ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE",
                    error_msg_clean))

})


test_that("An error is triggered when filter_depend_cols is incomplete with check_cross_domain_adam_dependencies disables", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb_08.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |> expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(".*ADLB spec: ADLB.USUBJIDto execute: ADLB.ADLB filter ADLB.AGE",
                    error_msg_clean))

})
