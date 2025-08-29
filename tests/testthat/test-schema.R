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
    validate_yaml("domain_schema", use_yq = FALSE) |>
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
    validate_yaml("domain_schema", use_yq = FALSE) |>
    expect_message("v YAML file '[a-zA-Z0-9.]+' is valid!")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/table_metadata: Property 'forbidden_extra_field' is not allowed")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/table_metadata: Property 'another_forbidden' is not allowed")

  unlink(yaml_file)
})


test_that("Missing field in table_metadata fails yaml validation checks", {

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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/table_metadata: Property 'forbidden_field' is not allowed")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/table_metadata: Missing required field 'table'")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/init: Property 'forbidden_extra_field' is not allowed")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/init: Missing required field 'filter_depend_cols'")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0: Property 'invalid_property' is not allowed")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/1: Missing required field 'column'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/1: Property 'col' is not allowed")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/2: Expected type 'object', must be object")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/3: Missing required field 'column'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/4: Missing required field 'column'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/5: Missing required field 'column'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/6/column: Expected type 'string', must be string")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/7/column: Expected type 'string', must be string")

  # validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
  #   expect_error("/column_metadata/8/column: Expected type 'string', must not be quoted")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0/parameters/0: Expected type 'object', must be object")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0/parameters/1: Expected type 'object', must be object")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0/parameters/2: Expected type 'object', must be object")


  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0/parameters: Expected type 'array', must be array")

  unlink(yaml_file)
})


test_that("Too many specifications for column fail yaml validation checks", {

  skip(message = "Need to implement custom business rules validation")

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
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0: Cannot have 'code_id' and 'source' at the same time")

  unlink(yaml_file)
})


test_that("Columns parameters with no code_id fail yaml validation checks", {

  skip(message = "Need to implement custom business rules validation")

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
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata/0: Cannot have 'parameters' without 'code_id'")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/column_metadata: Expected type 'array', must be array")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("Root level: Missing required field 'column_metadata'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("Root level: Property 'column_metadata_wrong' is not allowed")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions: Expected type 'array', must be array")

  unlink(yaml_file)
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
  - id2: X
    code_id: fnc1
"
  yaml_file <- create_temp_yaml(yaml_content)

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0: Missing required field 'id'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0: Property 'id2' is not allowed")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0/parameters: Expected type 'array', must be array")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0/parameters/0/parm1: Expected type 'string', must be string")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0/parameters/1: Expected type 'object', must be object")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0/parameters/2: Expected type 'object', must be object")

  unlink(yaml_file)
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

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("YAML validation failed for [a-zA-Z0-9.]+")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0: Missing required field 'id'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0: Missing required field 'code_id'")

  validate_yaml(yaml_file, "domain_schema", use_yq = FALSE) |>
    expect_error("/row_actions/0: Property 'id2' is not allowed")

  unlink(yaml_file)
})
