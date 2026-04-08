# should_use_method_as_depend_cols -------------------------------------------

test_that("method is ignored when it should not create dependencies", {
  # method is NULL
  expect_null(should_use_method_as_depend_cols(NULL, list(), "AGE", "ADSL"))
  # component$id is present
  expect_null(should_use_method_as_depend_cols(
    "DM.AGE",
    list(id = "cmp"),
    "AGE",
    "ADSL"
  ))
  # method matches col_id (no prefix)
  expect_null(should_use_method_as_depend_cols("AGE", list(), "AGE", "ADSL"))
  # method matches domain + col_id (with prefix)
  expect_null(should_use_method_as_depend_cols(
    "ADSL.AGE",
    list(),
    "AGE",
    "ADSL"
  ))
})

test_that("method is used when it references a different column or domain", {
  # same domain, different column
  expect_equal(
    should_use_method_as_depend_cols("ADSL.RFSTDTC", list(), "AGE", "ADSL"),
    "ADSL.RFSTDTC"
  )
  # different domain
  expect_equal(
    should_use_method_as_depend_cols("DM.AGE", list(), "AGE", "ADSL"),
    "DM.AGE"
  )
  # different column, no prefix
  expect_equal(
    should_use_method_as_depend_cols("RFSTDTC", list(), "AGE", "ADSL"),
    "RFSTDTC"
  )
  # different domain and rename of column (should still use method)
  expect_equal(
    should_use_method_as_depend_cols("DM.AGE", list(), "AGEGR", "ADSL"),
    "DM.AGE"
  )
})
