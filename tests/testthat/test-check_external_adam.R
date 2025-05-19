test_that(
  "No errors occur for a complete specification when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_01.yml"))
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
  "No errors occur for a complete specification when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_01.yml"))
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
  "An error occurs for an incomplete specification of external adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_02.yml"))
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
      regexp = ".*ADaM spec:\n\tadsl\\.AGE\n\tadsl\\.STUDYID\n\tadsl\\.USUBJID\nto execute:\n\tadlb\\.AGE")
  }
)

test_that(
  "No errors occur for an incomplete specification of external adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_02.yml"))
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
  "An error occurs for an incomplete specification of internal adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_03.yml"))
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
      regexp =  ".*ADaM spec for adlb:\n\tadlb.VISITNUM\nto execute:\n\tadlb\\.VISITNUM2")
  }
)

test_that(
  "An error occurs for an incomplete specification of internal adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_03.yml"))
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
      regexp = ".*ADaM spec:\n\tadlb.VISITNUM\nto execute:\n\tadlb\\.VISITNUM2")

  }
)

test_that(
  "An error occurs for an incomplete specification of both internal and external adam columns when check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_04.yml"))
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
    ) |> expect_error(regexp = ".*ADaM spec for adlb:\n\tadlb\\.STUDYID\n\tadlb\\.VISITNUM\nto execute:\n\tadlb\\.AGE\n\tadlb\\.VISITNUM2")
  }
)

test_that(
  "An error occurs for an incomplete specification of both internal and external adam columns when check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_04.yml"))
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
    ) |> expect_error(regexp = paste0("The following columns are missing in the ADaM spec:",
                                      "\n\tadlb\\.STUDYID\n\tadlb\\.VISITNUM\n\tadsl\\.AGE",
                                      "\n\tadsl\\.STUDYID\n\tadsl\\.USUBJID",
                                      "\nto execute:\n\tadlb\\.AGE\n\tadlb\\.VISITNUM2"))
  }
)

test_that(
  "An error occurs for ADSL dependency on filter when ADSL columns are not specified and check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_05.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(regexp = paste0(".*ADaM spec:",
                                      "\n\tADSL\\.SEX\n\tADSL\\.STUDYID\n\tADSL\\.USUBJID",
                                      "\nto execute:\n\tADLB filter"))

  }
)

test_that(
  "No errors occur for ADSL dependency on filter when ADSL columns are not specified and check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "check_external_adam_adlb_05.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
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
  "An error occurs for ADSL dependency on filter and action when ADSL columns are not specified and check_external_adam is enabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "check_external_adam_adlb_06.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(regexp =
                        paste0(".*ADaM spec:",
                               "\n\tADSL\\.AGE\n\tADSL\\.SEX\n\tADSL.\\STUDYID\n\tADSL\\.USUBJID",
                               "\nto execute:\n\tADLB filter\n\tADLB\\.AGE"))
  }
)

test_that(
  "No errors occur for ADSL dependency on filter and action when ADSL columns are not specified and check_external_adam is disabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "check_external_adam_adlb_06.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
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
  "An error occurs for ADSL dependency on filter for two domains when ADSL columns are not specified and check_external_adam is enabled",
  {
    # SETUP
    ui_path <- c(
      test_path("fixtures", "check_external_adam_adlb_06.yml"),
      test_path("fixtures", "check_external_adam_advs_01.yml")
    )
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
      path_ui_data = ui_path,
      code_component_source_files =  std_lib_path,
      path_trial_metadata = path_trial_metadata,
      path_domain_keys = domain_keys_path,
      path_output = output_path,
      data_connection = "pharmaverse",
      check_external_adam = TRUE
    ) |> expect_error(regexp =
                      ".*ADaM spec:\n\tADSL\\.AGE\n\tADSL\\.SEX\n\tADSL\\.STUDYID\n\tADSL\\.USUBJID\nto execute:\n\tADLB filter\n\tADLB\\.AGE\n\tADVS filter")
  }
)

test_that(
  "No error occur for ADSL dependency on filter for two domains when ADSL columns are not specified and check_external_adam is disabled",
  {
    # SETUP
    ui_path <- c(
      test_path("fixtures", "check_external_adam_adlb_06.yml"),
      test_path("fixtures", "check_external_adam_advs_01.yml")
    )
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

    domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
    output_path <- withr::local_tempdir()

    # EXPECT
    actual <- generate_adam_code(
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






