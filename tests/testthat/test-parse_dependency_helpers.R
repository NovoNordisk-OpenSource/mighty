test_that("is_row_dependency identifies row/parameter references", {
  deps <- c("rows.FILTER1", "parameters.P1", "SEX", "ADSL.AGE")

  expect_equal(
    is_row_dependency(deps),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("has_domain_prefix identifies qualified column refs", {
  deps <- c("rows.FILTER1", "parameters.P1", "SEX", "ADSL.AGE")

  expect_equal(
    has_domain_prefix(deps),
    c(FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("extract_dependency_id extracts identifiers", {
  deps <- c("SEX", "ADSL.AGE", "rows.FILTER1")

  expect_equal(
    extract_dependency_id(deps),
    c("SEX", "AGE", "FILTER1")
  )
})

test_that("extract_domain_prefix extracts domain from qualified deps", {
  deps <- c("ADSL.AGE", "ADAE.AESTDT", "DM.USUBJID")

  expect_equal(
    extract_domain_prefix(deps),
    c("ADSL", "ADAE", "DM")
  )
})

test_that("qualify_column_refs creates qualified references", {
  # Test with ADaM and SDTM dependencies
  deps <- data.table::data.table(
    domain = c("ADSL", "DM", "ADAE"),
    column_name = c("AGE", "USUBJID", "AESTDT")
  )

  result <- qualify_column_refs(deps)

  expect_equal(result, c("ADSL.AGE", "DM.USUBJID", "ADAE.AESTDT"))
})

test_that("qualify_column_refs handles empty data frames", {
  empty_deps <- data.table::data.table(
    domain = character(0),
    column_name = character(0)
  )

  result <- qualify_column_refs(empty_deps)

  expect_equal(result, character(0))
})

test_that("qualify_column_refs works with single row", {
  single_dep <- data.table::data.table(
    domain = "ADSL",
    column_name = "USUBJID"
  )

  result <- qualify_column_refs(single_dep)

  expect_equal(result, "ADSL.USUBJID")
})
