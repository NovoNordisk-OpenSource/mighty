test_that("No filters", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adsl_01.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])

  # Check external dependencies
  exp_external_dep <- data.table(
    domain = c("DM", "DM", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 4),
    column_name = c("ARM", "USUBJID", "ARM", "USUBJID")
  )
  act_external_dep <- actual$program_sequence$external_dependencies_by_program[[1]]
  expect_equal(act_external_dep, exp_external_dep)
})

test_that("No domain filters", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adsl_02.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])

  # Check external dependencies
  exp_external_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 8),
    column_name = c("ARM", "DOMAIN", "STUDYID", "USUBJID", "ARM", "DOMAIN", "STUDYID", "USUBJID")
  )
  act_external_dep <- actual$program_sequence$external_dependencies_by_program[[1]]
  expect_equal(act_external_dep, exp_external_dep)
})

test_that("No global filters", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adsl_03.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])

  # Check external dependencies
  exp_external_dep <-  data.table(
    domain = c("DM", "DM", "DM", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 6),
    column_name = c("AGEU", "ARM", "USUBJID", "AGEU", "ARM", "USUBJID")
  )
  act_external_dep <- actual$program_sequence$external_dependencies_by_program[[1]]
  expect_equal(act_external_dep, exp_external_dep)
})

test_that("No filters and no derivations", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adsl_04.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check external dependencies
  exp_external_dep <- data.table(
    domain = c("DM", "DM", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 4),
    column_name = c("ARM", "USUBJID", "ARM", "USUBJID")
  )
  act_external_dep <- actual$program_sequence$external_dependencies_by_program[[1]]
  expect_equal(act_external_dep, exp_external_dep)
})

test_that("Global filter and domain filter", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adsl_05.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])

  # Check external dependencies
  exp_external_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM",
               "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 10),
    column_name = c("AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID")
  )
  act_external_dep <- actual$program_sequence$external_dependencies_by_program[[1]]
  expect_equal(act_external_dep, exp_external_dep)

})

test_that("Global filter and domain filter incl. ADSL dependencies", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adlb_01.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse",
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check data model
  expect_equal(actual$data_model$type, "domain_init")
  actual$data_model$depend_cols[[1]] |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

  # Check casing
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_true(any(grepl("ADSL <- readRDS\\(", programs[["1_ADLB.R"]])))
  expect_true(any(grepl("ADLB <- dplyr::left_join\\(ADLB, ADSL, by",
                        programs[["1_ADLB.R"]])))

  # Check external dependencies
  actual$program_sequence$external_dependencies_by_program[[1]] |>
    dplyr::arrange(domain, domain_type, column_name) |>
    dplyr::relocate(domain, domain_type, column_name) |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

})

test_that("Global filter and domain filter incl. adsl dependencies (lower case)", {

  # SETUP
  ui_path <- testthat::test_path("fixtures", "column_dependencies_adlb_02.yml")
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adlb_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse",
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check data model
  expect_equal(actual$data_model$type, "domain_init")
  actual$data_model$depend_cols[[1]] |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

  # Check casing
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_true(any(grepl("adsl <- readRDS\\(", programs[["1_adlb.R"]])))
  expect_true(any(grepl("adlb <- dplyr::left_join\\(adlb, adsl, by",
                        programs[["1_adlb.R"]])))

  # Check external dependencies
  actual$program_sequence$external_dependencies_by_program[[1]] |>
    dplyr::arrange(domain, domain_type, column_name) |>
    dplyr::relocate(domain, domain_type, column_name) |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

})

test_that("Check external predecessor", {

  # SETUP
  ui_path <- test_path("fixtures", "column_dependencies_adsl_06.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )

  # EXPECT

  # Check data model
  actual$data_model$depend_cols[[1]] |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

  # Check external dependencies
  actual$program_sequence$external_dependencies_by_program[[1]] |>
    dplyr::arrange(domain, domain_type, column_name) |>
    dplyr::relocate(domain, domain_type, column_name) |>
    as.data.frame() |> expect_snapshot_value(style = "json2")

})

test_that("Error is triggered when both a col_mutate and a col_copy action exist that inputs a core variable and return the ADaM variable of same name", {

  # SETUP
  ui_path <- test_path("fixtures", "column_dependencies_adsl_07.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT/EXPECT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  ) |> expect_error(regexp = "Column\\(s\\) AGE are outputted in multiple actions in domain ADSL.")

})


test_that("Error is triggered when multiple col_compute actions exist that inputs a core variable and return the ADaM variable of same name", {

  # SETUP
  ui_path <- test_path("fixtures", "column_dependencies_adsl_08.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT/EXPECT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  ) |> expect_error(regexp = "Column\\(s\\) AGE are outputted in multiple actions in domain ADSL.")

})

test_that("Dependencies between a col_compute action that inputs/returns a core variable and other action that also inputs the same core variable is handled correctly", {

  # SETUP
  ui_path <- test_path("fixtures", "column_dependencies_adsl_09.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check edges
  actual$edges |>
    data.table::setorder(node_id, parent_node) |>
    as.data.frame() |>
    expect_snapshot_value(style = "json2")
})

test_that("Dependencies between a col_compute action that inputs/returns multiple core variable and other action that also inputs the one of the core variables is handled correctly", {

  # SETUP
  ui_path <- test_path("fixtures", "column_dependencies_adsl_10.yml")
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT
  actual <- generate_adam_code(
    path_ui_data = ui_path,
    code_component_source_files =  std_lib_path,
    path_trial_metadata = path_trial_metadata,
    path_domain_keys = domain_keys_path,
    path_output = output_path,
    data_connection = "pharmaverse"
  )

  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check generated ADSL
  x[[1]] |> source()
  setcolorder(ADSL, sort(names(ADSL)))
  ADSL |> expect_snapshot_value(style = "json2")

  # Check edges
  actual$edges |>
    data.table::setorder(node_id, parent_node) |>
    as.data.frame() |>
    expect_snapshot_value(style = "json2")
})



