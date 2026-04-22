# Test Setup
# This file runs once before all tests

# warn=2 converts warnings to errors
withr::local_options(
  .new = list(
    warn = 2,
    connector.verbosity_level = "quiet",
    mighty.metadata.verbosity_level = "quiet",
    styler.quiet = TRUE
  ),
  .local_envir = testthat::teardown_env()
)
