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

  # Check edges
  expected_edges <-  data.table(
    parent_node = c("ADSL-PLANNED_ARM", "ADSL-preprocess_domain"),
    node_id = c("ADSL-ARM_GRP1-arm_group_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 4),
    column_name = c("ARM", "USUBJID", "ARM", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1-arm_group_01", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID"))
  expect_equal(nrow(ADSL), 308)
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = c("ADSL-PLANNED_ARM", "ADSL-preprocess_domain"),
    node_id = c("ADSL-ARM_GRP1-arm_group_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 8),
    column_name = c("ARM", "DOMAIN", "STUDYID", "USUBJID", "ARM", "DOMAIN", "STUDYID", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1-arm_group_01", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID"))
  expect_equal(nrow(ADSL), 306)
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = c("ADSL-PLANNED_ARM", "ADSL-preprocess_domain"),
    node_id = c("ADSL-ARM_GRP1-arm_group_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <-   expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 6),
    column_name = c("ARM", "DTHFL", "USUBJID", "ARM", "DTHFL", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1-arm_group_01", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID"))
  expect_equal(nrow(ADSL), 305)
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = c("ADSL-preprocess_domain"),
    node_id = c("ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 4),
    column_name = c("ARM", "USUBJID", "ARM", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("ARM", "PLANNED_ARM", "USUBJID"))
  expect_equal(nrow(ADSL), 308)
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = c("ADSL-PLANNED_ARM", "ADSL-preprocess_domain"),
    node_id = c("ADSL-ARM_GRP1-arm_group_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM",
               "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 10),
    column_name = c("AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "ADSL-ARM_GRP1-arm_group_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-ARM_GRP1-arm_group_01", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID"))
  expect_equal(nrow(ADSL), 306)
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = character(),
    node_id = character()
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("ADSL", "ADSL", "ADSL", "LB", "LB", "LB", "LB"),
    domain_type = c("adam", "adam", "adam", "sdtm", "sdtm", "sdtm", "sdtm"),
    column_name = c("SEX", "STUDYID", "USUBJID", "LBSTRESN", "STUDYID", "USUBJID", "VISITNUM")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADLB", programs[["1_ADLB.R"]])
  expect_section_order("Initialize ADLB", "Preprocess ADLB", programs[["1_ADLB.R"]])
  expect_section_order("Preprocess ADLB", "Save ADLB", programs[["1_ADLB.R"]])
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

  # Check edges
  expected_edges <-  data.table(
    parent_node = character(),
    node_id = character()
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("adsl", "adsl", "adsl", "lb", "lb", "lb", "lb"),
    domain_type = c("adam", "adam", "adam", "sdtm", "sdtm", "sdtm", "sdtm"),
    column_name = c("SEX", "STUDYID", "USUBJID", "LBSTRESN", "STUDYID", "USUBJID", "VISITNUM")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADLB", programs[["1_adlb.R"]])
  expect_section_order("Initialize ADLB", "Preprocess ADLB", programs[["1_adlb.R"]])
  expect_section_order("Preprocess ADLB", "Save ADLB", programs[["1_adlb.R"]])
})

test_that("External predecessor dependencies are handled correctly", {

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
  write_adam_programs(dir = output_path, programs = actual$programs)
  x <- list.files(output_path, full.names = TRUE)

  # EXPECT

  # Check edges
  expected_edges <- data.table(
    parent_node = c("adsl-preprocess_domain", "adsl-PLANNED_ARM", "adsl-preprocess_domain"),
    node_id = c("adsl-PLANNED_ARM", "adsl-PLANNED_ARM2", "adsl-VAR1")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM",
               "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE",
               "EX", "EX"),
    domain_type = rep("sdtm", 12),
    column_name = c("AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "USUBJID", "VAR1")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_adsl.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_adsl.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_adsl.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "Save ADSL", programs[["1_adsl.R"]])
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

  # Check edges
  expected_edges <- data.table(
    parent_node = c("ADSL-preprocess_domain", "ADSL-AGE2-age2_01", "ADSL-preprocess_domain",
                    "ADSL-AGE-age_redefined_01", "ADSL-preprocess_domain"),
    node_id = c("ADSL-AGE-age_redefined_01", "ADSL-AGE-age_redefined_01",
                "ADSL-AGE2-age2_01", "ADSL-AGE3-age3_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM", "DM",
               "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE",
               "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 12),
    column_name = c("AGE", "AGEU", "ARM", "RACE", "SEX", "USUBJID",
                    "AGE", "AGEU", "ARM", "RACE", "SEX", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-AGE2-age2_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE2-age2_01", "ADSL-AGE-age_redefined_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE-age_redefined_01", "ADSL-AGE3-age3_01", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE3-age3_01", "Save ADSL", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-PLANNED_ARM", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("USUBJID", "AGE", "AGE2", "AGE3", "PLANNED_ARM", "ARM", "RACE", "SEX"))
  expect_equal(nrow(ADSL), 308)
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

  # Check edges
  expected_edges <- data.table(
    parent_node = c("ADSL-preprocess_domain", "ADSL-AGE2-age2_01", "ADSL-preprocess_domain",
                    "ADSL-AGE-SEX-age_sex_redefined_01", "ADSL-AGE2-age2_01"),
    node_id = c("ADSL-AGE-SEX-age_sex_redefined_01", "ADSL-AGE-SEX-age_sex_redefined_01",
                "ADSL-AGE2-age2_01", "ADSL-AGE3-age3_01", "ADSL-AGE4-age4_01")
  )
  expect_equal(actual$edges, expected_edges)

  # Check external dependencies
  expected_ext_dep <- data.table(
    domain = c("DM", "DM", "DM", "DM", "DM",
               "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE", "DM_VACCINE"),
    domain_type = rep("sdtm", 10),
    column_name = c("AGE", "AGEU", "RACE", "SEX", "USUBJID",
                    "AGE", "AGEU", "RACE", "SEX", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL", "Preprocess ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Preprocess ADSL", "ADSL-AGE2-age2_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE2-age2_01", "ADSL-AGE-SEX-age_sex_redefined_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE-SEX-age_sex_redefined_01", "ADSL-AGE3-age3_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE2-age2_01", "ADSL-AGE4-age4_01", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE3-age3_01", "Save ADSL", programs[["1_ADSL.R"]])
  expect_section_order("ADSL-AGE4-age4_01", "Save ADSL", programs[["1_ADSL.R"]])

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),  c("USUBJID", "AGE", "AGE2", "AGE3", "AGE4", "RACE", "SEX"))
  expect_equal(nrow(ADSL), 308)
})
