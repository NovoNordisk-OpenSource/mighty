test_that("Valid trial metadata YAML passes schema validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  EX:
    - USUBJID
  DS:
    - USUBJID
    - DSSEQ
primary_keys_adam:
  ADSL:
    - USUBJID
    - STUDYID
  ADLB:
    - USUBJID
    - PARAMCD
primary_keys_md:
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
primary_keys_sdtm:
  LB:
    - USUBJID
    - LBSEQ
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
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
primary_keys_sdtm:
  EX:
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
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


test_that("Empty primary_keys sections fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm | Error message: Expected type 'object', must be object"
    )
})


test_that("Invalid domain names in primary_keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  invalid_domain:
    - USUBJID
  '123INVALID':
    - STUDYID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm | Error message: Property 'invalid_domain' is not allowed"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm | Error message: Property '123INVALID' is not allowed"
    )
})


test_that("Invalid variable names in primary keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  EX:
    - USUBJID
    - invalid_var
    - '123INVALID'
    - ''
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm -> EX -> item 2 | Error message: String does not match pattern"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm -> EX -> item 3 | Error message: String does not match pattern"
    )

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm -> EX -> item 4 | Error message: String is too short"
    )
})


test_that("Empty primary key arrays fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  EX:
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm -> EX | Error message: Expected type 'array', must be array"
    )
})


test_that("Duplicate variable names in primary keys fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  EX:
    - USUBJID
    - EXSEQ
    - USUBJID
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm -> EX | Error message: Array items are not unique"
    )
})


test_that("Primary keys sections with no domains fail validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '0001'
project_id: '1234'
complete_id: '0001-1234'
instance: 'current'
primary_keys_sdtm:
  {}
primary_keys_adam:
  ADSL:
    - USUBJID
primary_keys_md:
  MDPARAM:
    - STUDYID
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  validate_yaml(yaml_file, "trial_metadata_schema", use_yq = TRUE) |>
    expect_error(
      "Error location: primary_keys_sdtm | Error message: Object does not have enough properties"
    )
})


test_that("Complex valid trial metadata passes validation", {
  # SETUP ----------------------------------------------------------------------

  yaml_content <- "
trial_id: '9999'
project_id: '5678abc'
complete_id: '9999-5678abc'
instance: 'production_v2'
primary_keys_sdtm:
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
primary_keys_adam:
  ADSL:
    - USUBJID
    - STUDYID
    - SITEID
  ADLB:
    - USUBJID
    - PARAMCD
    - AVISITN
    - ADT
  ADAE:
    - USUBJID
    - TRTA
    - AESEQ
  ADTTE:
    - USUBJID
    - PARAMCD
    - CNSR
primary_keys_md:
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
primary_keys_sdtm:
  A:
    - A
  Z9_TEST:
    - VAR_123
    - TEST_VAR_ABC
primary_keys_adam:
  AD123:
    - USUBJID123
    - TEST_123_ABC
primary_keys_md:
  MD_COMPLEX_NAME:
    - COMPLEX_VAR_NAME_123
"
  yaml_file <- create_temp_yaml(yaml_content, name = "temp_trial_metadata.yml")

  # ACT / ASSERT ---------------------------------------------------------------

  yaml_file |>
    validate_yaml("trial_metadata_schema", use_yq = TRUE) |>
    expect_error(class = "validation_error")
})
