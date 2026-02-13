test_that("Error when domain keys are defined in both trial metadata and ADaM specification", {
  # SETUP -------------------------------------------------------------------
  trial_path <- withr::local_tempdir()
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  adam_specifications <- test_path("fixtures", "skeleton_adsl.yml")
  standards_lib <- "mighty.standards"

  # ACT & EXPECT ------------------------------------------------------------

  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = trial_path
    ),
    "Domains have keys defined in both trial metadata and domain specifications"
  )
})

test_that("collate_primary_keys combines keys from both sources when no duplicates", {
  # SETUP -------------------------------------------------------------------
  ui_yml <- list(adlb = list(keys = c("USUBJID", "PARAMCD")))
  trial_metadata_keys <- list(keys = list(EX = c("USUBJID")))

  # ACT ---------------------------------------------------------------------
  result <- collate_primary_keys(ui_yml, trial_metadata_keys)

  # EXPECT ------------------------------------------------------------------
  expect_length(result, 2)
  expect_equal(result$EX, c("USUBJID"))
  expect_equal(result$ADLB, c("USUBJID", "PARAMCD"))
})

test_that("collate_primary_keys converts domain names to uppercase", {
  # SETUP -------------------------------------------------------------------
  ui_yml <- list(
    adsl = list(keys = c("USUBJID"))
  )

  trial_metadata_keys <- list(
    keys = list(
      ex = c("USUBJID"),
      DM = c("USUBJID") #
    )
  )

  # ACT ---------------------------------------------------------------------
  result <- collate_primary_keys(ui_yml, trial_metadata_keys)

  # EXPECT ------------------------------------------------------------------
  expect_length(result, 3)
  expect_equal(result$EX, c("USUBJID"))
  expect_equal(result$DM, c("USUBJID"))
  expect_equal(result$ADSL, c("USUBJID"))
})
