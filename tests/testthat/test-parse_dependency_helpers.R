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
