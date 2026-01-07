test_that("Supplementary data action is placed after filter_domain when no supplementary columns are used in filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- test_path("fixtures", "supplementary_data_adsl_01.yml")
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
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

  # Check program order
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-EFFICACY-SAFETY",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-EFFICACY-SAFETY",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(
    names(ADSL),
    c(
      "STUDYID",
      "USUBJID",
      "ARM",
      "PLANNED_ARM",
      "ARM_GRP1",
      "EFFICACY",
      "SAFETY",
      "AGEU",
      "DOMAIN"
    )
  )
})

test_that("Supp data action is placed before filter_domain when supp columns are used in filters (1 domain filter)", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- test_path("fixtures", "supplementary_data_adsl_02.yml")
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
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

  # Check program order
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-EFFICACY-SAFETY",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-EFFICACY-SAFETY",
    "ADSL-filter_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 234)
  expect_setequal(
    names(ADSL),
    c(
      "STUDYID",
      "USUBJID",
      "ARM",
      "PLANNED_ARM",
      "ARM_GRP1",
      "EFFICACY",
      "SAFETY",
      "AGEU",
      "DOMAIN"
    )
  )
})


test_that("Supp data action is placed before filter_domain when supp columns are used in filters (2 domain filters)", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- test_path("fixtures", "supplementary_data_adsl_03.yml")
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
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

  # Check program order
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )

  expect_section_order(
    "ADSL-init_domain",
    "ADSL-EFFICACY-SAFETY",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-EFFICACY-SAFETY",
    "ADSL-filter_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADSL), 306)
  expect_setequal(
    names(ADSL),
    c(
      "STUDYID",
      "USUBJID",
      "ARM",
      "PLANNED_ARM",
      "ARM_GRP1",
      "EFFICACY",
      "SAFETY",
      "AGEU",
      "DOMAIN"
    )
  )
})


test_that("Supp data action is placed before filter_domain when supp columns and ADSL cols are used in filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- test_path("fixtures", "supplementary_data_adae_01.yml")
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
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

  # Check program order
  expect_section_order(
    "ADAE-1-read_data",
    "ADAE-init_domain",
    programs[["1_ADAE.R"]]
  )
  expect_section_order(
    "ADAE-init_domain",
    "ADAE-AETRTEM",
    programs[["1_ADAE.R"]]
  )
  expect_section_order(
    "ADAE-AETRTEM",
    "ADAE-filter_domain",
    programs[["1_ADAE.R"]]
  )

  # Check ADSL
  x[[1]] |> source()
  expect_equal(nrow(ADAE), 1126)
  expect_setequal(
    names(ADAE),
    c("STUDYID", "USUBJID", "AESOC", "AEBODSYS", "AETRTEM", "AGE", "AESEQ")
  )
})


test_that("Col_compute and parent col_compute is placed before filter_domain when the col_compute is used in filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- test_path("fixtures", "supplementary_data_adae_02.yml")
  adam_specifications_rendered <- setup_yml_file_for_testing(
    adam_specifications,
    environment()
  )
  path_trial_metadata <- test_path(
    "fixtures",
    "trial_metadata_lowercase_adsl.yml"
  )
  path_trial <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_trial,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighty.standards"

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications_rendered,
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

  # Check program order
  expect_section_order(
    "ADAE-1-read_data",
    "ADAE-init_domain",
    programs[["1_ADAE.R"]]
  )
  expect_section_order(
    "ADAE-init_domain",
    "ADAE-AESEV_GRP",
    programs[["1_ADAE.R"]]
  )
  expect_section_order(
    "ADAE-AESEV_GRP",
    "ADAE-AEBODSYS_GRP",
    programs[["1_ADAE.R"]]
  )
  expect_section_order(
    "ADAE-AEBODSYS_GRP",
    "ADAE-filter_domain",
    programs[["1_ADAE.R"]]
  )
  expect_section_order("ADAE-filter_domain", "ADAE-AGE", programs[["1_ADAE.R"]])

  # Check ADAE
  x[[1]] |> source()
  expect_equal(nrow(ADAE), 655)
  expect_setequal(
    names(ADAE),
    c(
      "STUDYID",
      "USUBJID",
      "AESEV",
      "AEBODSYS",
      "AGE",
      "AESEV_GRP",
      "AEBODSYS_GRP"
    )
  )
})
