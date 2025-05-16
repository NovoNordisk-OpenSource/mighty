test_that(
  "Check that no errors occur for a complete specification when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_01.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = FALSE
    ) |> expect_no_error()

  }
)

test_that(
  "Check that no errors occur for a complete specification when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_01.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_no_error()

  }
)

test_that(
  "Check that an error occurs for an incomplete specification of external adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_02.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(
      regexp = "The following columns are missing in the ADaM spec:\n\tadsl\\.AGE\n\tadsl\\.STUDYID\n\tadsl.USUBJID")
  }
)

test_that(
  "Check that no errors occur for an incomplete specification of external adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_02.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = FALSE
    ) |> expect_no_error()

  }
)

test_that(
  "Check that error occurs for an incomplete specification of internal adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_03.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = FALSE
    ) |> expect_error(
      regexp = "The following columns are missing in the ADaM spec for adlb:\n\tVISITNUM")
  }
)

test_that(
  "Check that error occurs for an incomplete specification of internal adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_03.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(
      regexp = "The following columns are missing in the ADaM spec:\n\tadlb.VISITNUM")

  }
)

test_that(
  "Check that an error occurs for an incomplete specification of both internal and external adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_04.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = FALSE
    ) |> expect_error(
      regexp = "The following columns are missing in the ADaM spec for adlb:\n\tSTUDYID\n\tVISITNUM")
  }
)

test_that(
  "Check that an error occurs for an incomplete specification of both internal and external adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "adlb_check_external_adam_04.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(
      regexp = "The following columns are missing in the ADaM spec:\n\tadlb\\.STUDYID\n\tadlb\\.VISITNUM\n\tadsl\\.AGE")
  }
)
