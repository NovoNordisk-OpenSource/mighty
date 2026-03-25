test_that("is_row_dependency identifies row/parameter references", {
  deps <- c("rows.filter1", "parameters.p1", "SEX", "adsl.AGE")

  expect_equal(
    is_row_dependency(deps),
    c(TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("has_domain_prefix identifies qualified column refs", {
  deps <- c("rows.filter1", "parameters.p1", "SEX", "adsl.AGE")

  expect_equal(
    has_domain_prefix(deps),
    c(FALSE, FALSE, FALSE, TRUE)
  )
})

test_that("extract_dependency_id extracts identifiers", {
  deps <- c("SEX", "adsl.AGE", "rows.filter1")

  expect_equal(
    extract_dependency_id(deps),
    c("SEX", "AGE", "filter1")
  )
})

test_that("extract_domain_prefix extracts domain from qualified deps", {
  deps <- c("adsl.AGE", "adae.AESTDT", "dm.USUBJID")

  expect_equal(
    extract_domain_prefix(deps),
    c("adsl", "adae", "dm")
  )
})

test_that("qualify_column_refs creates qualified references", {
  # Test with ADaM and SDTM dependencies
  deps <- data.table::data.table(
    domain = c("ADSL", "dm", "ADAE"),
    column_name = c("AGE", "USUBJID", "AESTDT")
  )

  result <- qualify_column_refs(deps)

  expect_equal(result, c("ADSL.AGE", "dm.USUBJID", "ADAE.AESTDT"))
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
