test_that("format_domain applies magenta color", {
  result <- format_domain("ADSL")
  expect_s3_class(result, "cli_ansi_string")
  expect_equal(as.character(result), "ADSL")
})

test_that("format_column applies green color", {
  result <- format_column("AGE")
  expect_s3_class(result, "cli_ansi_string")
  expect_equal(as.character(result), "AGE")
})

test_that("format_qualified_column applies cyan color", {
  result <- format_qualified_column("ADSL.AGE")
  expect_s3_class(result, "cli_ansi_string")
  expect_equal(as.character(result), "ADSL.AGE")
})

test_that("format_qualified_column validates qualification when strict=TRUE", {
  expect_error(
    format_qualified_column("AGE"),
    "requires domain-qualified references"
  )

  expect_error(
    format_qualified_column(c("ADSL.AGE", "USUBJID")),
    "Found unqualified column name"
  )

  # Works with strict=FALSE
  result <- format_qualified_column("AGE", strict = FALSE)
  expect_s3_class(result, "cli_ansi_string")
})

test_that("format_column_ref auto-detects qualification", {
  result_mixed <- format_column_ref(c("ADSL.AGE", "BRTHDTC"))
  expect_s3_class(result_mixed, "cli_ansi_string")
  expect_length(result_mixed, 2)
  expect_equal(as.character(result_mixed), c("ADSL.AGE", "BRTHDTC"))
})

test_that("format_list handles single item", {
  expect_equal(format_list("A"), "A")
})

test_that("format_list handles two items with 'and'", {
  expect_equal(format_list(c("A", "B")), "A and B")
})

test_that("format_list handles three or more items with Oxford comma", {
  expect_equal(format_list(c("A", "B", "C")), "A, B, and C")
  expect_equal(format_list(c("A", "B", "C", "D")), "A, B, C, and D")
})

test_that("format_list applies markup function to items", {
  result <- format_list(c("ADSL", "ADLB", "ADAE"), format_domain)
  expect_type(result, "character")
  expect_match(as.character(result), "ADSL, ADLB, and ADAE")
})

test_that("format_list validates inputs", {
  expect_error(format_list(character(0)), "Assertion on 'items' failed")
  expect_error(
    format_list(c("A", "B"), "not_a_function"),
    "Assertion on 'markup_fn' failed"
  )
})

test_that("format_dependencies_for_display handles mixed ADaM/SDTM dependencies", {
  result <- format_dependencies_for_display(c("ADSL.AGE", "DM.USUBJID"))
  expect_type(result, "character")
  expect_match(as.character(result), "ADSL\\.AGE and DM\\.USUBJID")

  expect_equal(format_dependencies_for_display(character(0)), "None")
})
