test_that(
  "No error is triggered for a complete specification when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_01.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()

    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "No error is triggered for a complete specification when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_01.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_no_error()
    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "An error is triggered when cross-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_02.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(
      regexp = ".*ADaM spec:\n\tadsl\\.AGE\n\tadsl\\.STUDYID\n\tadsl\\.USUBJID\nto execute:\n\tadlb\\.AGE")
  }
)

test_that(
  "No error is triggered when cross-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_02.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()
    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "An error is triggered when within-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error(
      regexp =  ".*ADLB spec:\n\tadlb.VISITNUM\nto execute:\n\tadlb\\.VISITNUM2")
  }
)

test_that(
  "An error is triggered when within-domain ADaM specification is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(
      regexp = ".*ADaM spec:\n\tadlb.VISITNUM\nto execute:\n\tadlb\\.VISITNUM2")

  }
)

test_that(
  "An error is triggered when within-domain and cross-domain ADaM specifications are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_04.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error(regexp = ".*ADLB spec:\n\tadlb\\.STUDYID\n\tadlb\\.VISITNUM\nto execute:\n\tadlb\\.AGE\n\tadlb\\.VISITNUM2")
  }
)

test_that(
  "An error is triggered when within-domain and cross-domain ADaM specifications are incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_04.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(regexp = paste0("The following columns are missing in the ADaM spec:",
                                      "\n\tadlb\\.STUDYID\n\tadlb\\.VISITNUM\n\tadsl\\.AGE",
                                      "\n\tadsl\\.STUDYID\n\tadsl\\.USUBJID",
                                      "\nto execute:\n\tadlb\\.AGE\n\tadlb\\.VISITNUM2"))
  }
)

test_that(
  "An error is triggered when ADSL specification for filtering is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_05.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(regexp = paste0(".*ADaM spec:",
                                      "\n\tADSL\\.SEX\n\tADSL\\.STUDYID\n\tADSL\\.USUBJID",
                                      "\nto execute:\n\tADLB filter"))

  }
)

test_that(
  "No error is triggered when ADSL specification for filtering is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(test_path("fixtures", "assert_valid_adam_dependencies_adlb_05.yml"))
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()
    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "An error is triggered when ADSL specification for filtering and actions is incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(regexp =
                        paste0(".*ADaM spec:",
                               "\n\tADSL\\.AGE\n\tADSL\\.SEX\n\tADSL.\\STUDYID\n\tADSL\\.USUBJID",
                               "\nto execute:\n\tADLB filter\n\tADLB\\.AGE"))
  }
)

test_that(
  "No error is triggered when ADSL specification for filtering and actions is incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()
    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)


test_that(
  "An error is triggered when ADSL specifications for filtering in two domains are incomplete and when check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
    )
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(regexp =
                      ".*ADaM spec:\n\tADSL\\.AGE\n\tADSL\\.SEX\n\tADSL\\.STUDYID\n\tADSL\\.USUBJID\nto execute:\n\tADLB filter\n\tADLB\\.AGE\n\tADVS filter")
  }
)

test_that(
  "No error is triggered when ADSL specifications for filtering in two domains are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_06.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_01.yml")
    )
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_no_error()
    actual$data_model |> names() |> sort() |> expect_equal(c("code_id", "depend_cols", "depend_rows", "domain", "node_id", "outputs", "parameters", "type"))
  }
)

test_that(
  "An error is triggered when within-domain ADaM specifications for two domains are incomplete and when check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- c(
      test_path("fixtures", "assert_valid_adam_dependencies_adlb_03.yml"),
      test_path("fixtures", "assert_valid_adam_dependencies_advs_02.yml")
    )
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error(regexp = paste0(".*ADLB spec:\n\tadlb\\.VISITNUM\nto execute:",
                                      "\n\tadlb\\.VISITNUM2\n\n.*ADVS spec:",
                                      "\n\tADVS\\.VISITNUM\nto execute:\n\tADVS\\.VISITNUM2"))

  }
)

test_that(
  "An error is triggered when the within-domain ADaM specification that includes a standard component is incomplete and check_cross_domain_adam_dependencies is disabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_07.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = FALSE
    ) |> expect_error(regexp = paste0(".*ADLB spec:\n\tadlb\\.LBTEST\nto execute:",
                                      "\n\tadlb\\.LBTEST2\n\tadlb\\.LBTEST3\n\tadlb\\.LBTEST3_FLG"))
  }
)

test_that(
  "An error is triggered when the within-domain ADaM specification that includes a standard component is incomplete and check_cross_domain_adam_dependencies is enabled",
  {
    # SETUP
    ui_path <- test_path("fixtures", "assert_valid_adam_dependencies_adlb_07.yml")
    path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
    std_lib_path <- testthat::test_path("fixtures", "assert_valid_adam_dependencies_adlb.R")

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
      check_cross_domain_adam_dependencies = TRUE
    ) |> expect_error(regexp = paste0(".*ADaM spec:\n\tadlb\\.LBTEST\nto execute:",
                                      "\n\tadlb\\.LBTEST2\n\tadlb\\.LBTEST3\n\tadlb\\.LBTEST3_FLG"))
  }
)

