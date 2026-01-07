test_that("Complete spec passes when cross-domain disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_01.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})


test_that("Complete spec passes when cross-domain enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_01.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_no_error()

  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})


test_that("Incomplete cross-domain ADaM spec fails when checks enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_02.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE",
    error_msg_clean
  ))
})


test_that("Incomplete cross-domain ADaM spec passes when checks disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_02.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})


test_that("Incomplete within-domain ADaM spec fails when checks disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_03.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_snapshot_error()
})


test_that("Incomplete within-domain ADaM spec fails when checks enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_03.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_snapshot_error()
})


test_that("Incomplete within and cross-domain specs fail when checks disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_04.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADLB.STUDYID ADLB.VISITNUM ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE ADLB.VISITNUM2",
    error_msg_clean
  ))
})


test_that("Incomplete within and cross-domain specs fail when checks enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_04.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADLB.STUDYID ADLB.VISITNUM ADSL.AGE ADSL.STUDYID ADSL.USUBJIDto execute: ADLB.AGE ADLB.VISITNUM2",
    error_msg_clean
  ))
})


test_that("Incomplete ADSL filter spec fails when cross-domain enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_05.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter",
    error_msg_clean
  ))
})


test_that("Incomplete ADSL filter spec passes when cross-domain disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_05.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()
  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})


test_that("Incomplete ADSL filter and actions fail when checks enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_06.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE",
    error_msg_clean
  ))
})


test_that("Incomplete ADSL filter and actions pass when checks disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_06.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})


test_that("Incomplete ADSL filter in two domains fails when checks enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- c(
    test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
    test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE ADVS filter",
    error_msg_clean
  ))
})

test_that("Incomplete ADSL filter in two domains passes when checks disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- c(
    test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
    test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # EXPECT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  actual$actions |>
    names() |>
    sort() |>
    expect_equal(c(
      "code_id",
      "depend_cols",
      "depend_rows",
      "domain",
      "node_id",
      "outputs",
      "parameters",
      "type"
    ))
})

test_that("Incomplete within-domain in two domains fails when checks disabled", {
  # SETUP
  adam_specifications <- c(
    test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml"),
    test_path("fixtures", "assert_valid_adam_dependencies_advs_02.yml")
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADLB spec: ADLB.VISITNUMto execute: ADLB.VISITNUM2.*ADVS spec: ADVS.VISITNUMto execute: ADVS.VISITNUM2",
    error_msg_clean
  ))
})

test_that("Incomplete within-domain spec w/ component fails when x-check disabled", {
  # SETUP
  adam_specifications <- test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_07.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))

  expect_true(grepl(
    ".* ADLB spec: ADLB.LBTESTto execute: ADLB.LBTEST2 ADLB.LBTEST3 ADLB.LBTEST3_FLG",
    error_msg_clean
  ))
})


test_that("Incomplete within-domain spec w/ component fails when x-check enabled", {
  # SETUP
  adam_specifications <- test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_07.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADLB.LBTESTto execute: ADLB.LBTEST2 ADLB.LBTEST3 ADLB.LBTEST3_FLG",
    error_msg_clean
  ))
})


test_that("Incomplete filter_depend_cols fails when x-check enabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_08.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADaM spec: ADLB.USUBJID ADSL.AGE ADSL.SEX ADSL.STUDYID ADSL.USUBJIDto execute: ADLB filter ADLB.AGE",
    error_msg_clean
  ))
})


test_that("Incomplete filter_depend_cols fails when x-check disabled", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- testthat::test_path(
    "fixtures",
    "assert_valid_adam_dependencies_adlb_08.yml"
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("lb")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  error_msg <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_error()

  # EXPECT -------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl(
    ".*ADLB spec: ADLB.USUBJIDto execute: ADLB.ADLB filter ADLB.AGE",
    error_msg_clean
  ))
})
