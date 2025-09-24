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
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
  AVAL:
    code_id: '2'
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("domain_schema", use_yq = TRUE) |>
    expect_message("v YAML file 'temp_test_file.yml' is valid!")
})


test_that("Extra fields in table fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
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
column_action:
  STUDYID:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table | Error message: Property 'forbidden_extra_field' is not allowed"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table | Error message: Property 'another_forbidden' is not allowed"
    )


})


test_that("Missing field AND invalid field fails yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
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
column_action:
  - column: STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table | Error message: Missing required field 'name'"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: table | Error message: Property 'forbidden_field' is not allowed"
    )


})


test_that("Extra field in init fails yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
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
column_action:
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
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
column_action:
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
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
    - col
  AVAL:
    code_iid: 'A'
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()

})


test_that("Invalid parameter specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
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
      "Error location: column_metadata → VAR1 → parameters → item 1 | Error message: Expected type 'object' but got character"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → VAR1 → parameters → item 2 | Error message: Expected type 'object' but got character"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → VAR1 → parameters → item 3 | Error message: Expected type 'object' but got character"
    )
})


test_that("Missing parameter specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
    code_id: fnc1
    parameters:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_metadata → VAR1 → parameters | Error message: Expected type 'array' but got logical"
    )
})


test_that("Too many specifications for column fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
    code_id: fnc1
    source: VAR0
  VAR2:
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
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
    parameters:
      - parm1: A
  VAR2:
    parameters:
      - parm1: A
  VAR3:
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
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: column_action | Error message: Expected type 'array', must be array"
    )
})


test_that("Invalid root property fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
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
  STUDYID:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Required field 'column_action' is missing. Please add this field")

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Unexpected field 'column_metadata_wrong' found")

})


test_that("Empty row_action fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
row_action:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: row_action | Error message: Expected type 'array', must be array"
    )
})


test_that("Invalid properties in row_action fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
row_action:
  A:
  X:
    code_idd: fnc1
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "row_action → X -> code_id | Error message: Required field 'code_id' is missing"
    )

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "row_action → A | Error message: Expected type 'object' but got list"
    )
})


test_that("Missing parameter specifications for row fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
row_action:
  X:
    code_id: fnc1
    parameters:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error(
      "row_action → X → parameters | Error message: Expected type 'array' but got logical"
    )
})


test_that("Invalid parameter specifications for row fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
row_action:
  X: 
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


test_that("Missing/invalid row fields fail yaml validation checks", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  STUDYID:
row_action:
  X:
    codee: 'A'
  Y:
    - code_id: 'b'
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()


})

test_that("Duplicate columns fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
  VAR1:
  VAR3:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Duplicate map key: 'VAR1'")

})


test_that("Duplicate row id's fail validation", {
  # SETUP ----------------------------------------------------------------------
  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
row_action:
  row_action_1:
    code_id: row_action_99
  row_action_1:
    code_id: row_action_00
  row_action_2:
    code_id: row_action_00
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("Duplicate map key: 'row_action_1'")

})

test_that("Multiple business logic validations fail", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
    parameters:
     - param1: 5
  VAR2:
    depend_rows:
      - A
  VAR3:
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_snapshot_error()

})


test_that("row_depends with missing row_action definition caught", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
table:
  name: ADLB
init:
  base_domains:
    - LB
  filter_domain:
    - LB: NA
  filter_global:
    - 'SAFFL == \"Y\"'
  filter_depend_cols:
    - SAFFL
column_action:
  VAR1:
    depend_rows: 
    - row_action_1
  VAR2:
    depend_rows: 
    - row_action_99
row_action:
  row_action_99:
    code_id: row_action_99
  row_action_00:
    code_id: row_action_00
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------
  
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("The following row actions are not defined, but are listed as row dependencies for either a column or another row action")
  validate_yaml(yaml_file, "domain_schema", use_yq = TRUE) |>
    expect_error("row_action_1")

})

