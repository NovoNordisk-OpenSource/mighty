test_that("Supplementary data action is placed after filter_domain when no supplementary columns are used in filters", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "supplementary_data_adsl_01.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
      testdata = "pharmaverse",
      test_data_path = path_trial,
      sdtm_domains = c("dm", "dm_vaccine", "suppdm" ,"suppdm_vaccine")
    )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
      path_ui_data = path_ui_data_rendered,
      standards_lib = standards_lib,
      path_trial_metadata = path_trial_metadata,
      path_trial = path_trial,
      check_cross_domain_adam_dependencies = TRUE
    )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # EXPECT ------------------------------------------------------------------

  path_comp <- paste0("-", test_path(), "/fixtures/components/")

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL",
                       paste0("ADSL-EFFICACY-SAFETY",
                              path_comp, "supp_dm_01.R"), programs[["1_ADSL.R"]])
  expect_section_order("Filter ADSL",
                       paste0("ADSL-EFFICACY-SAFETY",path_comp, "supp_dm_01.R"),
                       programs[["1_ADSL.R"]])
  expect_section_order("Filter ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1", "EFFICACY", "SAFETY", "AGEU", "DOMAIN"))
})


test_that("Supplementary data action is placed before filter_domain when supplementary columns are used in filters (1 domain filter)", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "supplementary_data_adsl_02.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm" ,"suppdm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # EXPECT ------------------------------------------------------------------

  path_comp <- paste0("-", test_path(), "/fixtures/components/")

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])
  expect_section_order("Initialize ADSL",
                       paste0("ADSL-EFFICACY-SAFETY",
                              path_comp, "supp_dm_01.R"), programs[["1_ADSL.R"]])
  expect_section_order(paste0("ADSL-EFFICACY-SAFETY",path_comp, "supp_dm_01.R"),
                       "Filter ADSL",
                       programs[["1_ADSL.R"]])
  expect_section_order("Filter ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 234)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1", "EFFICACY", "SAFETY", "AGEU", "DOMAIN"))

})


test_that("Supplementary data action is placed before filter_domain when supplementary columns are used in filters (2 domain filters)", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "supplementary_data_adsl_03.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm" ,"suppdm_vaccine")
  )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # EXPECT ------------------------------------------------------------------

  path_comp <- paste0("-", test_path(), "/fixtures/components/")

  # Check program order
  expect_section_order("Read data sets", "Initialize ADSL", programs[["1_ADSL.R"]])

  expect_section_order("Initialize ADSL",
                       paste0("ADSL-EFFICACY-SAFETY",
                              path_comp, "supp_dm_01.R"), programs[["1_ADSL.R"]])
  expect_section_order(paste0("ADSL-EFFICACY-SAFETY",path_comp, "supp_dm_01.R"),
                       "Filter ADSL",
                       programs[["1_ADSL.R"]])
  expect_section_order("Filter ADSL", "ADSL-PLANNED_ARM", programs[["1_ADSL.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(names(ADSL), c("STUDYID", "USUBJID", "ARM", "PLANNED_ARM", "ARM_GRP1", "EFFICACY", "SAFETY", "AGEU", "DOMAIN"))

})


test_that("Supplementary data action is placed before filter_domain when supplementary columns and ADSL cols are used in filters", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "supplementary_data_adae_01.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # EXPECT ------------------------------------------------------------------

  path_comp <- paste0("-", test_path(), "/fixtures/components/")

  # Check program order
  expect_section_order("Read data sets", "Initialize ADAE", programs[["1_ADAE.R"]])
  expect_section_order("Initialize ADAE",
                       paste0("ADAE-AETRTEM", path_comp, "supp_ae_01.R"),
                       programs[["1_ADAE.R"]])
  expect_section_order(paste0("ADAE-AETRTEM", path_comp, "supp_ae_01.R"),
                       "Filter ADAE",
                       programs[["1_ADAE.R"]])

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADAE), 1126)
  expect_setequal(names(ADAE), c("STUDYID", "USUBJID", "AESOC", "AEBODSYS", "AETRTEM", "AGE", "AESEQ"))
})


test_that("Col_compute and parent col_compute is placed before filter_domain when the col_compute is used in filters", {

  # SETUP -------------------------------------------------------------------

  path_ui_data <- test_path("fixtures", "supplementary_data_adae_02.yml")
  path_ui_data_rendered <- setup_yml_file_for_testing(path_ui_data, environment())
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighy.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    path_ui_data = path_ui_data_rendered,
    standards_lib = standards_lib,
    path_trial_metadata = path_trial_metadata,
    path_trial = path_trial,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_trial, programs = actual$programs)
  x <- list.files(path_trial, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  # EXPECT ------------------------------------------------------------------

  path_comp <- paste0("-", test_path(), "/fixtures/components/")

  # Check program order
  expect_section_order("Read data sets", "Initialize ADAE", programs[["1_ADAE.R"]])
  expect_section_order("Initialize ADAE",
                       paste0("ADAE-AESEV_GRP", path_comp, "aesev_grp_01.R"),
                       programs[["1_ADAE.R"]])
  expect_section_order(paste0("ADAE-AESEV_GRP", path_comp, "aesev_grp_01.R"),
                       paste0("AEBODSYS_GRP", path_comp, "aebodsys_grp_01.R"),
                       programs[["1_ADAE.R"]])
  expect_section_order(paste0("AEBODSYS_GRP", path_comp, "aebodsys_grp_01.R"),
                       "Filter ADAE",
                       programs[["1_ADAE.R"]])
  expect_section_order("Filter ADAE",
                       "ADAE-AGE-_col_echo",
                       programs[["1_ADAE.R"]])

  # Check ADAE
  x[[1]] |> source()
  expect_equal(nrow(ADAE), 655)
  expect_setequal(names(ADAE), c("STUDYID", "USUBJID", "AESEV", "AEBODSYS", "AGE", "AESEV_GRP", "AEBODSYS_GRP"))
})
