test_that("!expr path produces correct connector path expression", {
  result <- params_read_data_code(
    payload = c("DM.USUBJID"),
    domain = "ADAE",
    path_connector_config = '!expr here::here("_connector.yml")'
  )

  expect_equal(
    result$connector_path_expr,
    'here::here("_connector.yml")'
  )
})

test_that("make_connector_path_expr() returns quoted path for plain paths", {
  expect_equal(
    make_connector_path_expr("some/relative/path"),
    '"some/relative/path"'
  )
})

test_that("make_connector_path_expr() normalizes backslashes to forward slashes", {
  expect_equal(
    make_connector_path_expr("C:\\Users\\trial"),
    '"C:/Users/trial"'
  )
})

test_that("make_connector_path_expr() errors on empty string path", {
  expect_error(
    make_connector_path_expr(""),
    "path_connector_config"
  )
})
