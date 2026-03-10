test_that("Classifies SDTM domains correctly", {
  expect_equal(classify_data_domains("AE"), "sdtm")
  expect_equal(classify_data_domains("relrec"), "sdtm")
  expect_equal(classify_data_domains("suppdm"), "sdtm")
})

test_that("Classifies ADaM domains correctly", {
  expect_equal(classify_data_domains("adsl"), "adam")
})

test_that("Classifies metadata domains correctly", {
  expect_equal(classify_data_domains("mdcol"), "md")
})

test_that("Classification is case insensitive", {
  expect_equal(classify_data_domains("RELREC"), "sdtm")
  expect_equal(classify_data_domains("ADSL"), "adam")
  expect_equal(classify_data_domains("MDCOL"), "md")
})

test_that("Unrecognized domains return NA", {
  expect_equal(classify_data_domains("custom"), NA_character_)
})

test_that("Rejects invalid domain names", {
  expect_error(classify_data_domains("1abc"), "Invalid domain name")
  expect_error(classify_data_domains("a.b"), "Invalid domain name")
})

test_that("Handles vectorized input", {
  expect_equal(classify_data_domains(character(0)), character(0))
  expect_equal(
    classify_data_domains(c("lb", "ADVS", "mdcol", "foo")),
    c("sdtm", "adam", "md", NA_character_)
  )
})
