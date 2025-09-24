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
  base_domains:
    - DM

column_action:
  USUBJID:
    source: core.USUBJID
"

  trial_path <- withr::local_tempdir()
  ui_path <- file.path(trial_path, "invalid.yml")
  yml_no_table |>
  writeLines(ui_path)
  
  
    read_adam_specs(ui_path) |> expect_snapshot_error()
})



test_that("read_adam_specs errors on invalid filter specifications", {
  yml_invalid_filter <-  "
table:
  name: ADSL

init:
  base_domains:
    - DM
  filter_domain:
    - DM: NA
  filter_global:
    - 42  # Should be character or list
  filter_depend_cols:
    - NA

column_action:
  USUBJID:
"

  trial_path <- withr::local_tempdir()
  
  ui_path <- create_temp_yaml(yml_invalid_filter)
  expect_error(
    read_adam_specs(ui_path),
    "Expected type 'string"
  )
})
