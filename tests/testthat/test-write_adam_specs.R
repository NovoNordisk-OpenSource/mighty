test_that("Simple yaml specs are semantically identical", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  # TODO: Will we expect scenarios like this - if so, normalize function may
  # not work since the elements under a is an array:
  # yaml_non_normalized <- "
  # b:
  #   e: e_val
  # a:
  #   - f: f_val
  #   - d: d_val
  #     c: c_val
  # "
  yaml_non_normalized <- "
  b:
    e: e_val
  a:
    - d: d_val
      c: c_val
  "
  yaml_normalized <- "
  a:
    - c: c_val
      d: d_val
  b:
    e: e_val
  "

  # ACT ----------------------------------------------------------------------
  yaml_object_non_normalized <- yaml::yaml.load(yaml_non_normalized)
  yaml_object_now_normalized <- normalize_yaml_structure(
    yaml_object_non_normalized
  )
  yaml_object_normalized <- yaml::yaml.load(yaml_normalized)
  compare <- waldo::compare(yaml_object_now_normalized, yaml_object_normalized)

  # EXPECT -------------------------------------------------------------------
  expect_length(compare, 0)
})

test_that("Normalize is working as expected", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  yaml <- "
  table_metadata:
    table: ADSL

  column_action:
    USUBJID:
      source: core.USUBJID

  init:
    core_domains:
      - dm
  "
  normalized_yaml <- "
  column_action:
    USUBJID:
      source: core.USUBJID

  init:
    core_domains:
      - dm

  table_metadata:
    table: ADSL
  "
  # ACT ---------------------------------------------------------------------
  yaml_object <- yaml::yaml.load(yaml)
  normalized_yaml_object <- yaml::yaml.load(normalized_yaml)

  yaml_object_normalized <- normalize_yaml_structure(yaml_object)

  diffs <- waldo::compare(yaml_object_normalized, normalized_yaml_object)

  # EXPECT ------------------------------------------------------------------
  expect_length(diffs, 0)
  expect_equal(
    normalized_yaml_object$table_metadata,
    yaml_object_normalized$table_metadata
  )
  expect_equal(normalized_yaml_object$init, yaml_object_normalized$init)
  expect_equal(
    normalized_yaml_object$column_metadata,
    yaml_object_normalized$column_metadata
  )
})

test_that("Reading adam specs with code_id and writing back produces the same result", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  test_yaml <- "simple_incl_code_id_adsl.yml"
  ui_path <- c(
    test_path("fixtures", test_yaml)
  )
  adam_specs <- read_mighty_metadata_adam_domain(ui_path)
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
  write_adam_specs(adam_specs, output_path)

  yaml1 <- yaml::read_yaml(test_path("fixtures", test_yaml))
  yaml2 <- yaml::read_yaml(file.path(output_path, "ADSL.yml"))

  # normalize files to be able to compare
  yaml1_normalized <- normalize_yaml_structure(yaml1)
  yaml2_normalized <- normalize_yaml_structure(yaml2)
  # EXPECT ------------------------------------------------------------------
  compare <- waldo::compare(yaml1_normalized, yaml2_normalized)
  # expect file contents are identical
  expect_length(compare, 0)
})

test_that("Reading adam specs and writing back produces the same result", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  adam_specifications <- c(
    test_path("fixtures", "complex_adsl.yml"),
    test_path("fixtures", "complex_adlb.yml")
  )
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  adam_specs <- lapply(
    adam_specifications_rendered,
    read_mighty_metadata_adam_domain
  )
  output_path <- withr::local_tempdir()
  write_adam_specs(adam_specs, output_path)
  # ACT ---------------------------------------------------------------------

  yaml_expect_adsl <- yaml::read_yaml(setup_yml_file_for_testing(
    test_path("fixtures", "complex_adsl.yml"),
    environment()
  ))
  yaml_expect_adlb <- yaml::read_yaml(setup_yml_file_for_testing(
    test_path("fixtures", "complex_adlb.yml"),
    environment()
  ))
  yaml_actual_adsl <- yaml::read_yaml(file.path(output_path, "ADSL.yml"))
  yaml_actual_adlb <- yaml::read_yaml(file.path(output_path, "ADLB.yml"))

  # normalize files to be able to compare
  yaml_expect_adsl_normalized <- normalize_yaml_structure(yaml_expect_adsl)
  yaml_expect_adlb_normalized <- normalize_yaml_structure(yaml_expect_adlb)

  yaml_actual_adsl_normalized <- normalize_yaml_structure(yaml_actual_adsl)
  yaml_actual_adlb_normalized <- normalize_yaml_structure(yaml_actual_adlb)
  # EXPECT ------------------------------------------------------------------
  compare <- waldo::compare(
    yaml_expect_adsl_normalized,
    yaml_actual_adsl_normalized
  )
  # expect file contents are identical
  expect_length(compare, 0)

  compare <- waldo::compare(
    yaml_expect_adlb_normalized,
    yaml_actual_adlb_normalized
  )
  # expect file contents are identical
  expect_length(compare, 0)
})
