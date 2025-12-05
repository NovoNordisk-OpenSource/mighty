test_that("Too many specifications for column fail yaml validation checks", {
  skip("Validation for method + component.id not yet implemented")
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
keys: []
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
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_error(
      "The following columns have both `source` and `code_id` field populated"
    )
})


test_that("Columns parameters with no code_id fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
keys: []
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
    component:
      with:
        parm1: A
  - id: VAR2
    component:
      with:
        parm1: A
  - id: VAR3
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_error("The following columns have parameters but no code_id")
  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_error("VAR2")
})

test_that("Multiple business logic validations fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
keys: []
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
    component:
      with:
        param1: 5
  - id: VAR2
    depends:
      - rows.A
  - id: VAR3
rows: []
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_snapshot_error()
})


test_that("row_depends with missing row_action definition caught", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
id: ADLB
keys: []
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
  - id: row_action_00
    component:
      id: row_action_00
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_error(
      "The following row actions are not defined, but are listed as row dependencies for either a column or another row action" # nolint: line_length_linter
    )
  read_mighty_metadata_adam_domain(yaml_file) |>
    expect_error("row_action_1")
})
