# Test Setup
# This file runs once before all tests

withr::local_options(
  .new = list(
    connector.verbosity_level = "quiet",
    mighty.component.verbosity_level = "quiet",
    mighty.metadata.verbosity_level = "quiet",
    styler.quiet = TRUE
  ),
  .local_envir = testthat::teardown_env()
)

# warn=2 converts warnings to errors; not on CRAN, so new warnings from
# dependencies don't fail CRAN checks
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  withr::local_options(warn = 2, .local_envir = testthat::teardown_env())
}

# CRAN allows at most 2 cores during checks
old_dt_threads <- data.table::setDTthreads(2)
withr::defer(data.table::setDTthreads(old_dt_threads), testthat::teardown_env())
