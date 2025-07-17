test_that("Error is triggered for a col_copy and col_echo action having the same output", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adsl_01.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  expect_true(grepl("Column\\(s\\) ARM are outputted in multiple actions in domain ADSL.",
                    error_msg$message))
})

test_that("Error is triggered for a col_copy and col_compute action having an output in common", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adsl_02.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  expect_true(grepl("Column\\(s\\) AGE are outputted in multiple actions in domain ADSL.",
                    error_msg$message))
})


test_that("assert_valid_outputs passes when there are no duplicate columns within domains", {

  # ARRANGE -------------------------------------------------------------------

  x <- data.table::data.table(
    type = c("col_compute", "col_compute", "row_compute"),
    domain = c("ADSL", "ADSL", "ADSL"),
    depend_cols = list(c("USUBJID"), c("AGE"), c("SAFFL")),
    outputs = list(c("USUBJID", "AGE"), c("SEX", "RACE"), c())
  )

  # ACT & ASSERT --------------------------------------------------------------

  # Should not throw any error when columns are unique within domain
  expect_no_error(assert_valid_outputs(x))
})


test_that("assert_valid_outputs aggregates errors from single domain with duplicates", {

  # ARRANGE -------------------------------------------------------------------

  x <- data.table::data.table(
    type = c("col_compute", "col_compute"),
    domain = c("ADSL", "ADSL"),
    depend_cols = list(c("USUBJID"), c("AGE")),
    outputs = list(c("USUBJID", "AGE"), c("AGE", "SEX"))
  )

  # ACT & ASSERT --------------------------------------------------------------

  # Should throw error with specific message about duplicate column in ADSL domain
  expect_error(
    assert_valid_outputs(x),
    "Column\\(s\\) AGE are outputted in multiple actions in domain ADSL\\.",
    # Error should identify the specific duplicate column and domain
    fixed = FALSE
  )
})


test_that("assert_valid_outputs aggregates errors from multiple domains", {

  # ARRANGE -------------------------------------------------------------------
  x <- data.table::data.table(
    type = c("col_compute", "col_compute", "col_compute", "col_compute"),
    domain = c("ADSL", "ADSL", "ADAE", "ADAE"),
    depend_cols = list(c("USUBJID"), c("AGE"), c("USUBJID"), c("AEDECOD")),
    outputs = list(c("USUBJID", "AGE"), c("AGE", "SEX"), c("USUBJID", "AEDECOD"), c("AEDECOD", "AESEV"))
  )

  # ACT & ASSERT --------------------------------------------------------------

  # Should aggregate errors from both domains in multi-line error message
  error_msg <- expect_error(assert_valid_outputs(x))

  # Error message should contain both domain names since both have duplicates
  expect_true(grepl("ADSL", error_msg$message))
  expect_true(grepl("ADAE", error_msg$message))

  # Error message should mention both duplicate columns
  expect_true(grepl("AGE", error_msg$message))
  expect_true(grepl("AEDECOD", error_msg$message))

  # Should contain newline character indicating multiple errors were aggregated
  expect_true(grepl("\n", error_msg$message))
})


test_that("assert_valid_outputs aggregates errors from multiple domains", {

  # ARRANGE -------------------------------------------------------------------

  x <- data.table::data.table(
    type = c("column", "column", "column", "column"),
    domain = c("ADSL", "ADSL", "ADAE", "ADAE"),
    depend_cols = list(c("USUBJID"), c("AGE"), c("USUBJID"), c("AEDECOD")),
    outputs = list(
      c("USUBJID", "AGE"),
      c("AGE", "SEX"),
      c("USUBJID", "AEDECOD"),
      c("AEDECOD", "AESEV")
      ))

  # ACT & ASSERT --------------------------------------------------------------

  # Should aggregate errors from both domains in multi-line error message
  error_msg <- expect_error(assert_valid_outputs(x))

  # Error message should contain both domain names since both have duplicates
  expect_true(grepl("ADSL", error_msg$message))
  expect_true(grepl("ADAE", error_msg$message))

  # Error message should mention both duplicate columns
  expect_true(grepl("AGE", error_msg$message))
  expect_true(grepl("AEDECOD", error_msg$message))
})


test_that("assert_valid_outputs ignores row_compute actions in validation", {

  # ARRANGE -------------------------------------------------------------------

  x <- data.table::data.table(
    type = c("col_compute", "row_compute", "row_compute"),
    domain = c("ADSL", "ADSL", "ADSL"),
    depend_cols = list(c("USUBJID"), c("SAFFL"), c("SAFFL")),
    outputs = list(c("USUBJID"), c("USUBJID"), c("USUBJID"))
  )

  # ACT & ASSERT --------------------------------------------------------------

  # Should not error even though USUBJID appears in row_compute actions, since row_compute actions are excluded
  expect_no_error(assert_valid_outputs(x))
})


test_that("assert_valid_outputs treats NA type as column action", {
  # ARRANGE -------------------------------------------------------------------
  x <- data.table::data.table(
    type = c(NA, "col_compute"),
    domain = c("ADSL", "ADSL"),
    depend_cols = list(c("USUBJID"), c("AGE")),
    outputs = list(c("USUBJID"), c("USUBJID"))
  )

  # ACT & ASSERT --------------------------------------------------------------

  # Should treat NA type as column action and detect duplicate USUBJID
  expect_error(
    assert_valid_outputs(x),
    "Column\\(s\\) USUBJID are outputted in multiple actions in domain ADSL\\.",
    # NA type should be included in validation, causing duplicate detection
    fixed = FALSE
  )
})


test_that("Error is triggered for a col_compute action with invalid column name in UI data", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adsl_03.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT -------------------------------------------------------------------

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl("Expected column outputs:\"AGE\" Actual column outputs:\"AGE_INVALID\" Missing from expected outputs:AGE",
                    error_msg_clean))
})


test_that("Error is triggered for a col_compute with two outputs that only have one output specified in the UI data", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "assert_valid_outputs_adlb_01.yml")
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

  error_msg <-
    generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    ) |>
    expect_error()

  # EXPECT --------------------------------------------------------------------

  error_msg_clean <- gsub("\\s+", " ", gsub("\\n", "", error_msg$message))
  expect_true(grepl("Expected column outputs:\"AVALFL\",\"AVALREA\" Actual column outputs:\"AVALFL\" Missing from expected outputs:AVALREA",
                    error_msg_clean))

})

