domain_keys <- list(
  ADSL = c("STUDYID", "USUBJID"),
  ADLB = c("STUDYID", "USUBJID", "PARAMCD")
)
init_metadata_simple <- list(
  filter_domain = list(LB = NA_character_),
  filter_global = NA_character_,
  filter_depend_cols = list(NA_character_)
)

test_that("empty keep_vars returns NULL (falsy for whisker) and has_keep_vars FALSE", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_null(result$keep_vars)
  expect_false(result$has_keep_vars)
})

test_that("keep_vars returns character vector and has_keep_vars TRUE", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = c("USUBJID", "PARAMCD", "AVAL"),
    domain_keys = domain_keys
  )

  expect_equal(result$keep_vars, c("USUBJID", "PARAMCD", "AVAL"))
  expect_true(result$has_keep_vars)
})

test_that("SRC_ is excluded from keep_vars", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = c("USUBJID", "SRC_", "PARAMCD"),
    domain_keys = domain_keys
  )

  expect_false("SRC_" %in% result$keep_vars)
  expect_equal(result$keep_vars, c("USUBJID", "PARAMCD"))
  expect_true(result$has_keep_vars)
})

test_that("joins contains select_expr and keys as character vectors", {
  init_metadata <- list(
    filter_domain = list(LB = NA_character_),
    filter_global = NA_character_,
    filter_depend_cols = list("ADSL.SEX")
  )

  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_length(result$joins, 1)
  expect_equal(result$joins[[1]]$table, "ADSL")
  expect_equal(result$joins[[1]]$select_expr, c("SEX", "STUDYID", "USUBJID"))
  expect_equal(result$joins[[1]]$keys, c("STUDYID", "USUBJID"))
})

test_that("multiple global filters are collapsed into one string", {
  init_metadata <- init_metadata_simple
  init_metadata$filter_global <- c("!is.na(SEX)", "AGE > 18")

  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_equal(result$global_filter, "!is.na(SEX), AGE > 18")
})

test_that("a single global filter is returned unchanged", {
  init_metadata <- init_metadata_simple
  init_metadata$filter_global <- "!is.na(SEX)"

  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_equal(result$global_filter, "!is.na(SEX)")
})

test_that("an NA global filter returns NULL", {
  result <- params_domain_filter_code(
    .self = "ADLB",
    init_metadata = init_metadata_simple,
    keep_vars = character(0),
    domain_keys = domain_keys
  )

  expect_null(result$global_filter)
})
