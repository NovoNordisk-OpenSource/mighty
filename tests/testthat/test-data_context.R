test_that("as_data_context() returns NULL and data_context objects unchanged", {
  expect_null(as_data_context(NULL))

  dc <- data_context$new()
  expect_identical(as_data_context(dc), dc)
})

test_that("as_data_context() wraps a connectors object in a data_context", {
  skip_if_not_installed("connector")
  skip_if_not_installed("arrow")

  sdtm_path <- withr::local_tempdir()
  arrow::write_parquet(
    data.frame(USUBJID = "01", AGE = 30),
    file.path(sdtm_path, "dm.parquet")
  )
  cnt <- connector::connectors(sdtm = connector::connector_fs(sdtm_path))

  dc <- as_data_context(cnt)

  expect_true(inherits(dc, "data_context"))
  expect_true(dc$has_variables("DM", c("USUBJID", "AGE"), "sdtm"))
})

test_that("as_data_context() rejects input that is not a connectors object", {
  expect_snapshot(as_data_context(list()), error = TRUE)
  expect_snapshot(as_data_context("_connector.yml"), error = TRUE)
})

test_that("as_data_context() asks to pick one config from nested_connectors", {
  skip_if_not_installed("connector")

  cnt <- connector::nested_connectors(
    study_a = connector::connectors(
      sdtm = connector::connector_fs(withr::local_tempdir())
    ),
    study_b = connector::connectors(
      sdtm = connector::connector_fs(withr::local_tempdir())
    )
  )

  expect_snapshot(as_data_context(cnt), error = TRUE)
})
