test_that("Supplementary data is handled correctly", {

  # SETUP -------------------------------------------------------------------

  ui_path <- test_path("fixtures", "supplementary_data_adsl_01.yml")

  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")

  std_lib_path <- testthat::test_path("fixtures", "adsl_0001.R")

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT ---------------------------------------------------------------------

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

  # EXPECT ------------------------------------------------------------------

  # External dependencies
  expected_ext_dep <- data.table(
    domain = c("dm", "dm", "dm", "dm", "dm",
               "dm_vaccine", "dm_vaccine", "dm_vaccine", "dm_vaccine", "dm_vaccine",
               "suppdm", "suppdm", "suppdm", "suppdm", "suppdm",
               "suppdm_vaccine", "suppdm_vaccine", "suppdm_vaccine", "suppdm_vaccine", "suppdm_vaccine"),
    domain_type = rep("sdtm", 20),
    column_name = c("AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "AGEU", "ARM", "DOMAIN", "STUDYID", "USUBJID",
                    "QLABEL", "QNAM", "QVAL", "STUDYID", "USUBJID",
                    "QLABEL", "QNAM", "QVAL", "STUDYID", "USUBJID")
  )
  expect_equal(actual$program_sequence$input_cols[[1]],
               expected_ext_dep)

  # Edges
  expected_edges <- data.table(
    parent_node = c("ADSL-PLANNED_ARM", "ADSL-domain_init", "ADSL-domain_init"),
    node_id = c("ADSL-ARM_GRP1-arm_group_01", "ADSL-EFFICACY-SAFETY-supp_dm_01", "ADSL-PLANNED_ARM")
  )
  expect_equal(actual$edges,
               expected_edges)

  # Names of generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL),
                  c("STUDYID", "USUBJID", "ARM", "EFFICACY", "SAFETY", "PLANNED_ARM", "ARM_GRP1"))

})
