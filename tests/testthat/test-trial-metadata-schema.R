test_that("Valid trial metadata YAML passes schema validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:
    - USUBJID
  DS:
    - USUBJID
    - DSSEQ

  MDPARAM:
    - STUDYID
    - TOPICCD
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("trial_metadata_schema", use_yq = TRUE) |>
    expect_no_error()
})


test_that("Trial metadata with project_id containing letters passes validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234mad'
complete_id: '0001-1234mad'
instance: 'draft'
keys:
  LB:
    - USUBJID
    - LBSEQ

  MDVISIT:
    - STUDYID
    - AVISITN
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("trial_metadata_schema", use_yq = TRUE) |>
    expect_no_error()
})


test_that("Invalid trial_id format fails validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '01'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: trial_id | Error message: String does not match required pattern"
    )
})


test_that("Invalid project_id format fails validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '123'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: project_id | Error message: String does not match required pattern defined by the following regular expression" # nolint: line_length_linter
    )
})


test_that("Invalid complete_id format fails validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '01-1234'
instance: 'current'
keys:
  EX:
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: complete_id | Error message: String does not match required pattern defined by the following regular expression" # nolint: line_length_linter
    )
})


test_that("Empty instance field fails validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: ''
keys:
  EX:
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: instance | Error message: String does not match required pattern defined by the following regular expression" # nolint: line_length_linter
    )
})


test_that("Missing required top-level fields fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
instance: 'current'
keys:
  EX:
    - USUBJID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------
  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_snapshot_error()
})


test_that("Extra fields at root level fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
extra_field: 'not allowed'
keys:
  EX:
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Root level -> extra_field | Error message: Unexpected field 'extra_field' found"
    )
})


test_that("Empty keys sections fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys | Error message: Expected type 'object', must be object"
    )
})


test_that("Invalid domain names in keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  invalid_domain:
    - USUBJID
  '123INVALID':
    - STUDYID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys | Error message: Property 'invalid_domain' is not allowed"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys | Error message: Property '123INVALID' is not allowed"
    )
})


test_that("Invalid domain names in keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:
    - USUBJID
    - invalid_var
    - '123INVALID'
    - ''

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys -> EX -> item 2 | Error message: String does not match pattern"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys -> EX -> item 3 | Error message: String does not match pattern"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys -> EX -> item 4 | Error message: String is too short"
    )
})


test_that("Empty key arrays fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys -> EX | Error message: Expected type 'array', must be array"
    )
})


test_that("Duplicate variable names in keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  EX:
    - USUBJID
    - USUBJID

  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys -> EX | Error message: Array items are not unique"
    )
})


test_that("Keys sections with no domains fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: keys | Error message: Object does not have enough properties"
    )
})


test_that("Complex valid trial metadata passes validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '9999'
project_id: '5678abc'
complete_id: '9999-5678abc'
instance: 'production_v2'
keys:
  DM:
    - USUBJID
    - DOMAIN
  EX:
    - USUBJID
    - EXSEQ
  VS:
    - USUBJID
    - VSTESTCD
    - VSDTC
  RELREC:
    - USUBJID
  AE:
    - USUBJID
    - AESEQ

  MDPARAM:
    - STUDYID
    - TOPICCD
    - PARAMCD
  MDVISIT:
    - STUDYID
    - AVISITN
    - VISITNUM
  MDFLOW:
    - STUDYID
    - AVISITN
    - TOPICCD
    - SRCDOM
  MDDERIVED:
    - DERIVED_ID
    - VERSION_ID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("trial_metadata_schema", use_yq = TRUE) |>
    expect_no_error()
})


test_that("Wrongly structured keys fails", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
keys:
  A:
    - A
  Z9_TEST:
    - VAR_123
    - TEST_VAR_ABC

  MD_COMPLEX_NAME:
    - COMPLEX_VAR_NAME_123
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("trial_metadata_schema", use_yq = TRUE) |>
    expect_error(class = "validation_error")
})
