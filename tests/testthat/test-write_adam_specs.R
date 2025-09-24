test_that("Simple yaml specs are semantically identical", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  # TODO: Will we expect scenarios like this - if so, normalise function may
  # not work since the elements under a is an array:
  # yaml_non_normalised <- "
  # b:
  #   e: eval
  # a:
  #   - f: fval
  #   - d: dval
  #     c: cval
  # "
  yaml_non_normalised <- "
  b:
    e: eval
  a:
    - d: dval
      c: cval
  "
  yaml_normalised <- "
  a:
    - c: cval
      d: dval
  b:
    e: eval
  "

  # ACT ----------------------------------------------------------------------
  yaml_object_non_normalised <- yaml::yaml.load(yaml_non_normalised)
  yaml_object_now_normalised <- normalise_yaml_structure(yaml_object_non_normalised)
  yaml_object_normalised <- yaml::yaml.load(yaml_normalised)
  compare <- waldo::compare(yaml_object_now_normalised, yaml_object_normalised)

  # EXPECT -------------------------------------------------------------------
  expect_length(compare, 0)
})

test_that("Normalise is working as expected", {
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
  normalised_yaml <- "
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
  normalised_yaml_object <- yaml::yaml.load(normalised_yaml)

  yaml_object_normalised <- normalise_yaml_structure(yaml_object)

  diffs <- waldo::compare(yaml_object_normalised, normalised_yaml_object)

  # EXPECT ------------------------------------------------------------------
  expect_length(diffs, 0)
  expect_equal(normalised_yaml_object$table_metadata, yaml_object_normalised$table_metadata)
  expect_equal(normalised_yaml_object$init, yaml_object_normalised$init)
  expect_equal(normalised_yaml_object$column_metadata, yaml_object_normalised$column_metadata)
})

test_that("Reading adam specs with code_id and writing back produces the same result", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  test_yaml <- "simple_incl_code_id_adsl.yml"
  ui_path <- c(
    test_path("fixtures", test_yaml)
  )
  adam_specs <- read_adam_specs(ui_path)
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------
  write_adam_specs(adam_specs, output_path)

  yaml1 <- yaml::read_yaml(test_path("fixtures", test_yaml))
  yaml2 <- yaml::read_yaml(file.path(output_path, "ADSL.yml"))

  # normalise files to be able to compare
  yaml1_nomalised <- normalise_yaml_structure(yaml1)
  yaml2_nomalised <- normalise_yaml_structure(yaml2)
  # EXPECT ------------------------------------------------------------------
  compare <- waldo::compare(yaml1_nomalised, yaml2_nomalised)
  # expect file contents are identical
  expect_length(compare, 0)
})

test_that("Reading adam specs and writing back produces the same result", {
  skip("MHzT: Writing specs back is not in scope yet")
  # SETUP -------------------------------------------------------------------
  path_ui_data  <- c(
    test_path("fixtures", "complex_adsl.yml"),
    test_path("fixtures", "complex_adlb.yml")
  )
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())  
  adam_specs <- read_adam_specs(path_ui_data_rendered)
  output_path <- withr::local_tempdir()
  write_adam_specs(adam_specs, output_path)
  # ACT ---------------------------------------------------------------------

  yaml_expect_adsl <- yaml::read_yaml(setup_yml_file_for_testing(test_path("fixtures", "complex_adsl.yml"), environment()))
  yaml_expect_adlb <- yaml::read_yaml(setup_yml_file_for_testing(test_path("fixtures", "complex_adlb.yml"), environment()))
  yaml_actual_adsl <- yaml::read_yaml(file.path(output_path, "ADSL.yml"))
  yaml_actual_adlb <- yaml::read_yaml(file.path(output_path, "ADLB.yml"))

  # normalise files to be able to compare
  yaml_expect_adsl_normalised <- normalise_yaml_structure(yaml_expect_adsl)
  yaml_expect_adlb_normalised <- normalise_yaml_structure(yaml_expect_adlb)

  yaml_actual_adsl_normalised <- normalise_yaml_structure(yaml_actual_adsl)
  yaml_actual_adlb_normalised <- normalise_yaml_structure(yaml_actual_adlb)
  # EXPECT ------------------------------------------------------------------
  compare <- waldo::compare(yaml_expect_adsl_normalised, yaml_actual_adsl_normalised)
  # expect file contents are identical
  expect_length(compare, 0)

  compare <- waldo::compare(yaml_expect_adlb_normalised, yaml_actual_adlb_normalised)
  # expect file contents are identical
  expect_length(compare, 0)

})
