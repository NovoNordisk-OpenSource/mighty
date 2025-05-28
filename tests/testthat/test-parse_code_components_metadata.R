test_that("parse metadata from source file", {
  # ARRANGE

  std_lib_path <- c(testthat::test_path("fixtures", "parse_code_components_metadata_2.R"))
  # ACT
  actual <- parse_code_components_metadata_from_file(std_lib_path)

  expected <- list(
    fn_AB = list(
      depend_cols = c("A", "B"),
      outputs = c("C"),
      type = "col_compute"
    ),
    fn_B = list(
      depend_cols = c("LB.A", "B"),
      outputs = c("C", "D"),
      type = "col_compute"
    )
  )

  # ASSERT
  expect_equal(actual, expected)

})
