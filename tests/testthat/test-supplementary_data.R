test_that("Supplementary data action is placed after filter_domain when no supplementary columns are used in filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "supplementary_data_adsl_01.yml",
      "_mighty" = "_mighty.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)
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

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "supplementary_data_adsl_02.yml",
      "_mighty" = "_mighty.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)
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

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "supplementary_data_adsl_03.yml",
      "_mighty" = "_mighty.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine", "suppdm", "suppdm_vaccine")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)
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

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adae" = "supplementary_data_adae_01.yml",
      "_mighty" = "_mighty_with_adsl_keys.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)
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

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adae" = "supplementary_data_adae_02.yml",
      "_mighty" = "_mighty_with_adsl_keys.yml"
    )
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("ae", "suppae"),
    adam_domains = c("adsl")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)
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
