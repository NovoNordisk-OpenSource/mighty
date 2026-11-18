test_that("generate_adam_code works offline with the bundled example study", {
  # SETUP -------------------------------------------------------------------

  # The example study lists only local component repos, so no network access
  # or GitHub token is needed. It is meant to run on CRAN.
  example_dir <- system.file("examples", "offline", package = "mighty")
  withr::local_dir(example_dir)

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(adam_specifications = "study_dir")

  # EXPECT ------------------------------------------------------------------

  expect_named(actual$programs, c("1_ADSL", "2_ADAE"))
  for (program in actual$programs) {
    expect_no_error(parse(text = program))
  }

  # Every standard component is rendered at least once
  expect_setequal(
    unlist(actual$program_sequence$code_id),
    c(
      "mighty_read_data",
      "mighty_init_domain",
      "mighty_filter_domain",
      "mighty_col_rename",
      "mighty_col_mutate",
      "mighty_col_echo",
      "mighty_write_data",
      "age_group_01.R"
    )
  )

  dir <- withr::local_tempdir()
  write_adam_programs(programs = actual$programs, dir = dir)
  expect_setequal(list.files(dir), c("1_ADSL.R", "2_ADAE.R"))
})
