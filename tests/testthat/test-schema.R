test_that("Valid yaml files pass yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  path_ui_data <- c(
    test_path("fixtures", "complex_adsl.yml"),
    test_path("fixtures", "complex_adlb.yml")
  )
  path_ui_data_rendered <- setup_yml_file_for_testing(
    path_ui_data,
    environment()
  )

  # ACT / ASSERT ---------------------------------------------------------------
  path_ui_data_rendered[[1]] |>
    validate_yaml("domain_schema") |>
    expect_message("v YAML file '[a-zA-Z0-9.]+' is valid!")

  path_ui_data_rendered[[1]] |>
    validate_yaml("domain_schema", use_yq = TRUE) |>
    expect_message("v YAML file '[a-zA-Z0-9.]+' is valid!")
})

test_that("Dummy metadata passes yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("domain_schema", use_yq = TRUE) |>
    expect_message("v YAML file 'temp_test_file.yml' is valid!")


})


test_that("Extra fields in table_metadata fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
  forbidden_extra_field: 'this should not exist'
  another_forbidden: 'neither should this'
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table_metadata | Error message: Property 'forbidden_extra_field' is not allowed"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table_metadata | Error message: Property 'another_forbidden' is not allowed"
    )


})


test_that("Missing field AND invalid field fails yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  forbidden_field: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table_metadata | Error message: Missing required field 'table'"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table_metadata | Error message: Property 'forbidden_field' is not allowed"
    )


})


test_that("Extra field in init fails yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
  forbidden_extra_field: 'this should not exist'
column_metadata:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: init | Error message: Property 'forbidden_extra_field' is not allowed"
    )


})


test_that("Missing field in init fails yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
column_metadata:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: init | Error message: Missing required field 'filter_depend_cols'"
    )


})


test_that("Invalid properties in column_metadata fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
    invalid_property: ABC
  - col: USUBJID
  - VISITNUM
  - code_id: fnc1
  - source: VISITNUM
  - parameters:
       - parm1: A
  - column:
  - column: 123
  - column: 'VAR2'
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()

})


test_that("Invalid parameter specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    code_id: fnc1
    parameters:
      - parm1
      - parm2 A
      -
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → item 1 → parameters → item 1 | Error message: Expected type 'object', must be object"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → item 1 → parameters → item 2 | Error message: Expected type 'object', must be object"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → item 1 → parameters → item 3 | Error message: Expected type 'object', must be object"
    )


})


test_that("Missing parameter specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    code_id: fnc1
    parameters:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → item 1 → parameters | Error message: Expected type 'array', must be array"
    )


})


test_that("Too many specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    code_id: fnc1
    source: VAR0
  - column: VAR2
    code_id: fnc1
    source: VAR0
  
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "The following columns have both `source` and `code_id` field populated"
    )


})


test_that("Columns parameters with no code_id fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    parameters:
      - parm1: A
  - column: VAR2
    parameters:
      - parm1: A
  - column: VAR3
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("The following columns have parameters but no code_id")
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("VAR2")

})


test_that("Empty column_metadata fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata | Error message: Expected type 'array', must be array"
    )


})


test_that("Invalid root property fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata_wrong:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Required field 'column_metadata' is missing. Please add this field")

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Unexpected field 'column_metadata_wrong' found")


})


test_that("Empty row_actions fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
row_actions:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: row_actions | Error message: Expected type 'array', must be array"
    )


})


test_that("Invalid properties in row_actions fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
row_actions:
  - id: A
  - id2: X
    code_id: fnc1
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: row_actions → A | Error message: Required field 'code_id' is missing"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: row_actions → item 2 | Error message: Unexpected field 'id2' found"
    )


})


test_that("Missing parameter specifications for row fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
row_actions:
  - id: X
    code_id: fnc1
    parameters:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: row_actions → item 1 → parameters | Error message: Expected type 'array', must be array"
    )


})


test_that("Invalid parameter specifications for row fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
row_actions:
  - id: X
    code_id: fnc1
    parameters:
      - parm1:
      - parm2 A
      -
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()


})


test_that("Missing row fields fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: STUDYID
row_actions:
  - id2: X
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()


})

test_that("Duplicate columns fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
  - column: VAR1
  - column: VAR3
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("The following columns are defined multiple times")
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("VAR1")

})


test_that("Duplicate row id's fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
row_actions:
  - id: row_action_1
    code_id: row_action_99
  - id: row_action_1
    code_id: row_action_00
  - id: row_action_2
    code_id: row_action_00
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("The following row id\\(s\\) are defined multiple times")
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("row_action_1")

})



test_that("Multiple business logic validations fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    parameters:
     - param1: 5
  - column: VAR1
  - column: VAR3
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()

})


test_that("row_depends with missing row_action definition caught", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table_metadata:
  table: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_metadata:
  - column: VAR1
    depend_rows: 
    - row_action_1
  - column: VAR2
    depend_rows: 
    - row_action_99
row_actions:
  - id: row_action_99
    code_id: row_action_99
  - id: row_action_00
    code_id: row_action_00
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("The following row actions are not defined, but are listed as row dependencies for either a column or another row action")
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("row_action_1")

})

