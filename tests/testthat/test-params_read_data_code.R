test_that("!expr path produces correct connector path expression", {
  result <- params_read_data_code(
    payload = c("dm.USUBJID"),
    domain = "adae",
    path_connector_config = "!expr here::here()"
  )

  expect_equal(
    result$connector_path_expr,
    'file.path(here::here(), "_connector.yml")'
  )
})

test_that("make_connector_path_expr() wraps plain paths in file.path()", {
  expect_equal(
    make_connector_path_expr("some/relative/path"),
    'file.path("some/relative/path", "_connector.yml")'
  )
})

test_that("make_connector_path_expr() normalizes backslashes to forward slashes", {
  expect_equal(
    make_connector_path_expr("C:\\Users\\trial"),
    'file.path("C:/Users/trial", "_connector.yml")'
  )
})

test_that("make_connector_path_expr() handles empty string path (#186)", {
  result <- make_connector_path_expr("")
  expect_equal(result, '"_connector.yml"')
})
