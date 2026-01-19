test_that("throw_yaml_validation_error throws error with correct class", {
  # ACT / ASSERT ---------------------------------------------------------------
  expect_error(
    throw_yaml_validation_error("test.yaml", "Error message"),
    class = "yaml_validation_error"
  )
})

test_that("throw_yaml_validation_error includes file name in error message", {
  # ACT / ASSERT ---------------------------------------------------------------
  expect_error(
    throw_yaml_validation_error("test.yaml", "Error message"),
    "test\\.yaml"
  )
})

test_that("throw_yaml_validation_error handles multiple messages", {
  # SETUP ----------------------------------------------------------------------
  messages <- c("Error 1", "Error 2", "Error 3")

  # ACT / ASSERT ---------------------------------------------------------------
  err <- tryCatch(
    throw_yaml_validation_error("test.yaml", messages),
    error = function(e) e
  )

  expect_s3_class(err, "yaml_validation_error")
  expect_match(conditionMessage(err), "Error 1")
  expect_match(conditionMessage(err), "Error 2")
  expect_match(conditionMessage(err), "Error 3")
})

test_that("throw_yaml_validation_error chains parent errors", {
  # SETUP ----------------------------------------------------------------------
  parent_error <- simpleError("Parent error message")

  # ACT / ASSERT ---------------------------------------------------------------
  err <- tryCatch(
    throw_yaml_validation_error(
      "test.yaml",
      "Child error",
      parent = parent_error
    ),
    error = function(e) e
  )

  expect_s3_class(err, "yaml_validation_error")
  expect_s3_class(err$parent, "error")
  expect_match(conditionMessage(err$parent), "Parent error message")
})

test_that("throw_yaml_validation_error includes standard validation message", {
  # ACT / ASSERT ---------------------------------------------------------------
  expect_error(
    throw_yaml_validation_error("test.yaml", "Error message"),
    "YAML validation failed"
  )
})
