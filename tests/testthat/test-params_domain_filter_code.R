domain_keys <- list(ADSL = c("STUDYID", "USUBJID"), ADLB = c("STUDYID", "USUBJID", "PARAMCD"))
init_metadata_simple <- list(
  filter_domain = list(LB = NA_character_),
  filter_global = NA_character_,
  filter_depend_cols = list(NA_character_)
)

test_that("empty keep_vars returns NULL (falsy for whisker)", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_null(result$keep_vars)
})

test_that("keep_vars returns comma-separated string", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = c("USUBJID", "PARAMCD", "AVAL"),
    domain_keys = domain_keys
  )

  expect_equal(result$keep_vars, "USUBJID, PARAMCD, AVAL")
})

test_that("SRC_ is excluded from keep_vars", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = c("USUBJID", "SRC_", "PARAMCD"),
    domain_keys = domain_keys
  )

  expect_false(grepl("SRC_", result$keep_vars))
  expect_match(result$keep_vars, "USUBJID")
  expect_match(result$keep_vars, "PARAMCD")
})
