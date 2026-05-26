domain_keys <- list(ADSL = c("STUDYID", "USUBJID"))
domain_ui_data <- list(
  columns = list(USUBJID = NULL, STUDYID = NULL, ARM = NULL, AGE = NULL)
)

test_that("non-final program returns keep_vars = NULL", {
  result <- params_write_domain_code(
    .self = "ADSL",
    is_final_pgm = FALSE,
    domain_keys = domain_keys,
    domain_ui_data = domain_ui_data,
    available_data = NULL
  )

  expect_null(result$keep_vars)
})

test_that("final program returns keep_vars as comma-newline separated string", {
  result <- params_write_domain_code(
    .self = "ADSL",
    is_final_pgm = TRUE,
    domain_keys = domain_keys,
    domain_ui_data = domain_ui_data,
    available_data = NULL
  )

  expect_equal(result$keep_vars, "USUBJID,\nSTUDYID,\nARM,\nAGE")
})

test_that("final program prefixes unavailable columns with # ", {
  available_data <- data.frame(
    domain = "ADSL",
    column_name = c("USUBJID", "STUDYID"),
    stringsAsFactors = FALSE
  )

  result <- params_write_domain_code(
    .self = "ADSL",
    is_final_pgm = TRUE,
    domain_keys = domain_keys,
    domain_ui_data = domain_ui_data,
    available_data = available_data
  )

  expect_match(result$keep_vars, "# ARM")
  expect_match(result$keep_vars, "# AGE")
  expect_match(result$keep_vars, "USUBJID")
  expect_match(result$keep_vars, "STUDYID")
})

test_that("final program with three or fewer columns prefixes unavailable columns with # ", {
  small_domain_ui_data <- list(columns = list(USUBJID = NULL, AGE = NULL))
  available_data <- data.frame(
    domain = "ADSL",
    column_name = "USUBJID",
    stringsAsFactors = FALSE
  )

  result <- params_write_domain_code(
    .self = "ADSL",
    is_final_pgm = TRUE,
    domain_keys = domain_keys,
    domain_ui_data = small_domain_ui_data,
    available_data = available_data
  )

  expect_equal(result$keep_vars, "\nUSUBJID\n# AGE\n")
})
