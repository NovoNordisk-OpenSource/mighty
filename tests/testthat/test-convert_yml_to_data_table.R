test_that("logical component parameters preserve type through conversion", {
  nested_list <- list(
    MY_COL = list(
      type = "col_compute",
      code_id = "my_component",
      parameters = list(list(domain = "ADSL", use_filter = FALSE))
    )
  )

  result <- convert_yml_to_data_table_(nested_list, domain = "ADLB")

  params <- result$parameters[[1]]
  expect_type(params$use_filter, "logical")
  expect_false(params$use_filter)
})
