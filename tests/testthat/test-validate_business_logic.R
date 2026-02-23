test_that("Column depends with wrong prefix should fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: USUBJID
  - id: VAR1
    depends:
      parameter.row_action_00
  - id: VAR2
    depends:
      row.row_action_99
rows:
  - id: row_action_99
    component:
      id: row_action_99
parameters:
  - id: row_action_00
    component:
      id: row_action_00
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)
  # ACT / ASSERT ---------------------------------------------------------------

  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error("YAML validation failed")
  expect_match(
    err_$parent$message,
    "/columns/1/depends must match pattern.*(rows|parameters)",
    all = FALSE
  )
})

test_that("Row depends with wrong prefix should fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: USUBJID
rows:
  - id: row_action_99
    component:
      id: row_action_99
    depends: parameter_action_00
parameters:
  - id: parameter_action_00
    component:
      id: parameter_action_00
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)
  # ACT / ASSERT ---------------------------------------------------------------

  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error("YAML validation failed")
  expect_match(
    err_$parent$message,
    "/rows/0/depends must match pattern.*(rows|parameters)",
    all = FALSE
  )
})

test_that("Parameter depends with wrong prefix should fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: USUBJID
rows:
  - id: row_action_99
    component:
      id: row_action_99
parameters:
  - id: parameter_action_00
    component:
      id: parameter_action_00
    depends: row_action_99
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)
  # ACT / ASSERT ---------------------------------------------------------------

  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error("YAML validation failed")
  expect_match(
    err_$parent$message,
    "/parameters/0/depends must match pattern.*(rows|parameters)",
    all = FALSE
  )
})

test_that("Dependencies with correct prefix should pass", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: USUBJID
  - id: VAR1
    depends: 
    -  parameters.parameter_action_00
  - id: VAR2
    depends: 
    -  rows.row_action_99
rows:
  - id: row_action_99
    component:
      id: row_action_99
    depends: 
    -  parameters.parameter_action_00
    -  parameters.parameter_action_01
parameters:    
  - id: parameter_action_00
    component:
      id: parameter_action_00
  - id: parameter_action_01
    component:
      id: parameter_action_01
    depends: 
    -  parameters.parameter_action_00
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)
  # ACT / ASSERT ---------------------------------------------------------------
  process_adam_domain(study$ADLB, "ADLB") |>
    expect_no_error()
})

test_that("Too many specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: VAR1
    method: VAR0
    component:
      id: fnc1
  - id: VAR2
    method: VAR0
    component:
      id: fnc1

"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  # ACT / ASSERT ---------------------------------------------------------------
  study <- mighty.metadata::mighty_study(adam_specifications)
  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error(
      "The following columns have both `method` and `component.id` field populated"
    )
  expect_match(err_$body, "- VAR1", all = FALSE)
  expect_match(err_$body, "- VAR2", all = FALSE)
})

test_that("row_depends with missing row_action definition caught", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
  global:
    - filter: 'SAFFL == \"Y\"'
      depends:
        - SAFFL
columns:
  - id: VAR1
    depends:
      - rows.row_action_1
  - id: VAR2
    depends:
      - rows.row_action_99
rows:
  - id: row_action_99
    component:
      id: row_action_99

parameters:
  - id: row_action_00
    component:
      id: row_action_00

"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  # ACT / ASSERT ---------------------------------------------------------------
  # Test that all validation errors are caught: Rules "val_depend_rows" and "val_keys_included_as_columns"
  study <- mighty.metadata::mighty_study(adam_specifications)
  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error(
      "The following row actions are not defined, but are listed as row dependencies for either a column or another row action" # nolint: line_length_linter
    )
  # Row action not defined:
  expect_match(err_$body, "- row_action_1", all = FALSE)
  # Keys defined but not defined as column
  expect_match(
    err_$body,
    "The following columns are specified as keys but are not defined in the columns section:",
    all = FALSE
  )
  expect_match(err_$body, "- USUBJID", all = FALSE)
})

test_that("duplicate column IDs cause validation error", {
  # SETUP ----------------------------------------------------------------------
  # This test verifies that duplicate column IDs are now caught by validation
  # before transformations can silently deduplicate them

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
columns:
  - id: USUBJID
  - id: VAR1
    component:
      id: first_component
  - id: VAR1
    component:
      id: second_component
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  # ACT / ASSERT ---------------------------------------------------------------
  # Test that validation error is caught: Rule "val_no_duplicate_columns"
  study <- mighty.metadata::mighty_study(adam_specifications)
  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error(
      "The following columns are defined multiple times"
    )
  expect_match(err_$body, "VAR1", all = FALSE)
})

test_that("keys not defined in columns fail validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADSL
label: Subject Level Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject
keys:
  - USUBJID
  - STUDYID

population:
  base:
    - domain: DM
      depends:
        - NA
      filter: NA

  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID
"
  adam_specifications <- setup_study_dir(list("adsl" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)

  # ACT / ASSERT ---------------------------------------------------------------
  # Test that validation error is caught: Rule "val_keys_included_as_columns"
  err_ <- process_adam_domain(study$ADSL, "ADSL") |>
    expect_error(
      "The following columns are specified as keys but are not defined in the columns section"
    )
  expect_match(err_$body, "STUDYID", all = FALSE)
})

test_that("unique column IDs pass validation", {
  # SETUP ----------------------------------------------------------------------
  # This test verifies that when columns have unique IDs, val_no_duplicate_columns()
  # returns list(valid = True, errors = character(0))

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
columns:
  - id: USUBJID
  - id: VAR1
    component:
      id: first_component
  - id: VAR2
    component:
      id: second_component
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)

  # ACT / ASSERT ---------------------------------------------------------------
  # Should pass validation without error
  expect_no_error(
    process_adam_domain(study$ADLB, "ADLB")
  )
})

test_that("duplicate row IDs cause validation error", {
  # SETUP ----------------------------------------------------------------------
  # This test verifies that duplicate row IDs are caught by validation.
  # Tests duplicates within rows section, within parameters section,
  # and across both sections.

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
      depends:
        - NA
      filter: NA
columns:
  - id: USUBJID
  - id: PARAM
rows:
  - id: duplicate_row_id
    component:
      id: first_component
parameters:    
  - id: duplicate_row_id
    component:
      id: second_component
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  # ACT / ASSERT ---------------------------------------------------------------
  # Test that validation error is caught: Rule "val_no_duplicate_row_parameter_ids"
  study <- mighty.metadata::mighty_study(adam_specifications)
  err_ <- process_adam_domain(study$ADLB, "ADLB") |>
    expect_error(
      "The following row or parameter id\\(s\\) are defined multiple times:"
    )
  expect_match(err_$body, "- duplicate_row_id", all = FALSE)
})

test_that("missing population section fails validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
columns:
  - id: USUBJID
"
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))
  study <- mighty.metadata::mighty_study(adam_specifications)

  # ACT / ASSERT ---------------------------------------------------------------
  process_adam_domain(study$ADLB, "ADLB") |>
    expect_error("YAML validation failed")

  process_adam_domain(study$ADLB, "ADLB") |>
    expect_error("must have required property 'population'")
})
