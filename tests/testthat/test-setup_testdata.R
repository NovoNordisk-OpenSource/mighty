testthat::test_that("SDTM test data is available", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
  setup_testdata(testdata = "pharmaverse", test_data_path = output_path)
  cnt <- connector::connect(config=file.path(output_path, "_connector.yml"))
  dm <- cnt$sdtm$tbl_cnt("dm.parquet")

  # EXPECT ------------------------------------------------------------------
  expect_equal(length(cnt$sdtm$list_content_cnt()), 6)
  expect_equal(length(cnt$adam$list_content_cnt()), 0)
  expect_equal(length(cnt$metadata$list_content_cnt()), 0)
  expect_equal(nrow(dm), 306)
  expect_equal(length(dm), 25)

})

testthat::test_that("ADaM test data is available", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
  setup_testdata(testdata = "pharmaverse", test_data_path = output_path, sdtm_domains = c(), adam_domains = c("adsl", "adae", "adlb"))
  cnt <- connector::connect(config=file.path(output_path, "_connector.yml"))
  adsl <- cnt$adam$tbl_cnt("adsl.parquet")

  # EXPECT ------------------------------------------------------------------
  expect_equal(length(cnt$sdtm$list_content_cnt()), 0)
  expect_equal(length(cnt$adam$list_content_cnt()), 3)
  expect_equal(length(cnt$metadata$list_content_cnt()), 0)
  expect_equal(nrow(adsl), 306)
})

testthat::test_that("SDTM and ADaM test data is available", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
  setup_testdata(testdata = "pharmaverse", test_data_path = output_path, sdtm_domains = c("dm"), adam_domains = c("adsl"))
  cnt <- connector::connect(config=file.path(output_path, "_connector.yml"))
  dm <- cnt$sdtm$tbl_cnt("dm.parquet")
  adsl <- cnt$adam$tbl_cnt("adsl.parquet")

  # EXPECT ------------------------------------------------------------------
  expect_equal(length(cnt$sdtm$list_content_cnt()), 1)
  expect_equal(length(cnt$adam$list_content_cnt()), 1)
  expect_equal(nrow(adsl), nrow(dm))

})

testthat::test_that("Non-existing SDTM domains cannot be prepared", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # EXPECT ------------------------------------------------------------------
  expect_message(setup_testdata(testdata = "pharmaverse",
                                test_data_path = output_path,
                                sdtm_domains = c("no_domain")),
                 paste0("Error writing dataset: 'no_domain' is not an ",
                        "exported object from 'namespace:pharmaversesdtm'\n",
                        "Please ensure the dataset no_domain exists in ",
                        "pharmaversesdtm."))
})

testthat::test_that("Non-existing ADaM domains cannot be prepared", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # EXPECT ------------------------------------------------------------------
  expect_message(setup_testdata(testdata = "pharmaverse",
                                test_data_path = output_path,
                                adam_domains = c("no_domain")),
                 paste0("Error writing dataset: 'no_domain' is not an ",
                        "exported object from 'namespace:pharmaverseadam'\n",
                        "Please ensure the dataset no_domain exists in ",
                        "pharmaverseadam."))
})

testthat::test_that("Non-existing testdata cannot be prepared", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()

  # EXPECT ------------------------------------------------------------------
  expect_error(setup_testdata(testdata = "non_existing_test_data",
                                test_data_path = output_path,
                                adam_domains = c("no_domain")),
                 paste0("'arg' should be \"pharmaverse\""))
})

