test_that("read_adam_specs handles missing files correctly", {
  # Test single missing file

    read_adam_specs("nonexistent_file.yml") |>
      expect_error(
        "The following ADaM Specification file(s) do not exist: 'nonexistent_file.yml'",
        fixed = TRUE
  )

  # Test multiple missing files
  missing_files <- c("missing1.yml", "missing2.yml", "missing3.yml")
  expect_error(
    read_adam_specs(missing_files),
    "The following ADaM Specification file(s) do not exist: 'missing1.yml', 'missing2.yml', 'missing3.yml'",
    fixed = TRUE
  )

  # Test mix of existing and missing files
  # Create a temporary file for testing

  temp_file <- withr::local_tempfile(fileext = ".yml")
  writeLines("test: content", temp_file)

  mixed_files <- c(temp_file, "missing1.yml", "missing2.yml")
  expect_error(
    read_adam_specs(mixed_files),
    "The following ADaM Specification file(s) do not exist: 'missing1.yml', 'missing2.yml'",
    fixed = TRUE
  )
})

test_that("read_adam_specs errors on missing required top-level elements", {
  # Missing table_metadata
  yml_no_table <- "
init:
  core_domains:
    - DM

column_metadata:
  - column: USUBJID
    source: core.USUBJID
"

  trial_path <- withr::local_tempdir()
  ui_path <- file.path(trial_path, "invalid.yml")
  yaml::read_yaml(text = yml_no_table) |>
    yaml::write_yaml(ui_path)

  expect_error(
    read_adam_specs(ui_path),
    "All elements in ui_yml must be named "
  )
})

test_that("read_adam_specs errors on empty columns", {
  yml_empty_columns <-  "
table_metadata:
  table: ADSL

init:
  core_domains:
    - DM

column_metadata: []
"

  trial_path <- withr::local_tempdir()
  ui_path <- file.path(trial_path, "invalid.yml")
  yaml::read_yaml(text = yml_empty_columns) |>
    yaml::write_yaml(ui_path)

  expect_error(
    read_adam_specs(ui_path),
    "'columns' element for domain 'ADSL' cannot be empty"
  )
})

test_that("read_adam_specs errors on invalid filter specifications", {
  yml_invalid_filter <-  "
table_metadata:
  table: ADSL

init:
  core_domains:
    - DM
  filter_global: 42  # Should be character or list

column_metadata:
  - column: USUBJID
    source: core.USUBJID
"

  trial_path <- withr::local_tempdir()
  ui_path <- file.path(trial_path, "invalid_filter.yml")
  yaml::read_yaml(text = yml_invalid_filter) |>
    yaml::write_yaml(ui_path)

  expect_error(
    read_adam_specs(ui_path),
    "filter_global in `init` section for domain 'ADSL' must be character vector or list"
  )
})
