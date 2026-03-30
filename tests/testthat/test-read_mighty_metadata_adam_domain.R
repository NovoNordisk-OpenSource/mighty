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
    - domain: DM
      depends:
        - NA
      filter: NA
columns:
  - id: USUBJID"

  adam_specifications <- setup_study_dir(list("adsl" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)

  result <- process_adam_domain(study$ADSL, "ADSL")
  expect_type(result, "list")
  expect_named(result, c("columns", "domain", "keys", "init"))
  expect_equal(result$domain, "ADSL")
})
