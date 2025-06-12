# Helper function to always show inline code using backticks
# ensures consistent in unit tests.
md_transform <- function(x) {
  gsub(
    pattern = "\\\\code\\{([^}]+)\\}",
    replacement = "`\\1`",
    x = x
  )
}

test_that("Documentation is created with custom tags", {
  withr::local_options(lifecycle_verbosity = "quiet")

  blocks <- test_path("cases", "good.R") |>
    roxygen2::parse_file() |>
    expect_no_error()

  results <- roxygen2::roclet_process(
    x = roxygen2::rd_roclet(),
    blocks = blocks
  ) |>
    expect_no_error()

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "man")) |>
    expect_true()

  rd_file <- roxygen2::roclet_output(
    x = roxygen2::rd_roclet(),
    results = results,
    base_path = tmp
  ) |>
    expect_no_error() |>
    suppressMessages()

  rd_file |>
    readLines() |>
    md_transform() |>
    cat(sep = "\n") |>
    expect_snapshot()
})
