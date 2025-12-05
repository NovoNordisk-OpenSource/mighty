test_that("has_content returns FALSE for NULL", {
  expect_false(mighty:::has_content(NULL))
})

test_that("has_content returns FALSE for all-NA vectors", {
  expect_false(mighty:::has_content(c(NA, NA)))
  expect_false(mighty:::has_content(c(NA_character_, NA_character_)))
  expect_false(mighty:::has_content(c(NA_real_, NA_real_)))
})

test_that("has_content returns TRUE for vectors with any non-NA value", {
  expect_true(mighty:::has_content(c(1, NA)))
  expect_true(mighty:::has_content(c(NA, 2)))
  expect_true(mighty:::has_content(c("a", NA)))
})

test_that("has_content returns FALSE for all-NA lists", {
  expect_false(mighty:::has_content(list(a = NA, b = NULL)))
  expect_false(mighty:::has_content(list(NA, NA, NA)))
  expect_false(mighty:::has_content(list()))
})

test_that("has_content returns TRUE for lists with any non-NA content", {
  expect_true(mighty:::has_content(list(a = 1, b = NA)))
  expect_true(mighty:::has_content(list(a = "text")))
})

test_that("has_content handles nested lists correctly", {
  # All NA/NULL nested
  expect_false(mighty:::has_content(list(a = list(b = NA, c = NULL))))

  # Some content in nested structure
  expect_true(mighty:::has_content(list(a = list(b = 1, c = NA))))
  expect_true(mighty:::has_content(list(list(NA), list(1))))
})

test_that("has_content handles data.frames correctly", {
  # All NA data frame
  expect_false(mighty:::has_content(data.frame(a = c(NA, NA))))

  # Data frame with some content
  expect_true(mighty:::has_content(data.frame(a = c(1, NA))))
  expect_true(mighty:::has_content(data.frame(a = 1, b = NA)))

  # Empty data frame
  expect_false(mighty:::has_content(data.frame()))
})

test_that("has_content handles zero-length vectors", {
  expect_false(mighty:::has_content(character(0)))
  expect_false(mighty:::has_content(numeric(0)))
  expect_false(mighty:::has_content(logical(0)))
  expect_false(mighty:::has_content(integer(0)))
})

test_that("has_content handles mixed content in complex structures", {
  # Population-like structure with all NA
  pop_all_na <- list(
    base = list(
      list(domain = NA, depends = NA, filter = NA)
    ),
    global = list(
      depends = NA,
      filter = NA
    )
  )
  expect_false(mighty:::has_content(pop_all_na$global))

  # Population-like structure with some content
  pop_with_content <- list(
    base = list(
      list(domain = "LB", depends = character(0), filter = character(0))
    ),
    global = list(
      depends = c("ADSL.SAFFL"),
      filter = c("SAFFL == 'Y'")
    )
  )
  expect_true(mighty:::has_content(pop_with_content$global))
})
