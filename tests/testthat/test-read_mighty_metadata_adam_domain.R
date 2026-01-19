test_that("successful parsing works", {
  # Valid YAML with all required fields
  yaml_content <- "
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID]
population:
  base:
    - domain: dm
      depends:
        - NA
      filter: NA
columns:
  - id: USUBJID"

  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml_content, tmp)

  result <- read_mighty_metadata_adam_domain(tmp)
  expect_type(result, "list")
  expect_named(result, "ADSL")
})

test_that("mighty_metadata validation error includes file path", {
  # Create invalid YAML (missing required field)
  yaml_content <- "
id: ADSL
population:
  base:
    - domain: dm
columns:
  - id: USUBJID"

  tmp <- withr::local_tempfile(fileext = ".yml")
  writeLines(yaml_content, tmp)

  # Expect error message to include both filename and original error
  expect_error(
    read_mighty_metadata_adam_domain(tmp),
    "YAML validation failed for.*\\.yml",
    class = "yaml_validation_error"
  )

  # Verify original error message is preserved
  expect_error(
    read_mighty_metadata_adam_domain(tmp),
    "must have required property 'label'"
  )
})
