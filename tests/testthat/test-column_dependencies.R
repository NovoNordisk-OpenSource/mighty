test_that("No filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_01.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------
  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check outputs of read_data
  expected_outputs <- c(
    "DM.STUDYID",
    "DM.USUBJID",
    "DM.ARM",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.USUBJID",
    "DM_VACCINE.ARM"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_outputs)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-ARM_GRP1",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ARM_GRP1",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(
    names(ADSL),
    c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID", "STUDYID")
  )
  expect_equal(nrow(ADSL), 308)
})


test_that("No filters - external core domain dependency on col_compute action", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_06.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check outputs of read_data
  expected_outputs <- c(
    "DM.USUBJID",
    "DM.STUDYID",
    "DM.ARM",
    "DM.ACTARM",
    "DM_VACCINE.USUBJID",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.ARM",
    "DM_VACCINE.ACTARM"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_outputs)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-ARM_GRP1",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ARM_GRP1",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-ACTARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ACTARM",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(
    names(ADSL),
    c("STUDYID", "ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID", "ACTARM")
  )
  expect_equal(nrow(ADSL), 308)
})


test_that("No domain filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_02.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "DM.USUBJID",
    "DM.DOMAIN",
    "DM.STUDYID",
    "DM.ARM",
    "DM_VACCINE.USUBJID",
    "DM_VACCINE.DOMAIN",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.ARM"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-filter_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-ARM_GRP1",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ARM_GRP1",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(
    names(ADSL),
    c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID", "DOMAIN", "STUDYID")
  )
  expect_equal(nrow(ADSL), 306)
})


test_that("No global filters", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_03.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "DM.STUDYID",
    "DM.USUBJID",
    "DM.DTHFL",
    "DM.ARM",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.USUBJID",
    "DM_VACCINE.DTHFL",
    "DM_VACCINE.ARM"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-filter_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-ARM_GRP1",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ARM_GRP1",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(
    names(ADSL),
    c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID", "DTHFL", "STUDYID")
  )
  expect_equal(nrow(ADSL), 305)
})


test_that("No filters and no derivations", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_04.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "DM.STUDYID",
    "DM.USUBJID",
    "DM.ARM",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.USUBJID",
    "DM_VACCINE.ARM"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(names(ADSL), c("ARM", "PLANNED_ARM", "USUBJID", "STUDYID"))
  expect_equal(nrow(ADSL), 308)
})


test_that("Global filter and domain filter", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "column_dependencies_adsl_05.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("dm", "dm_vaccine")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "DM.AGEU",
    "DM.ARM",
    "DM.DOMAIN",
    "DM.STUDYID",
    "DM.USUBJID",
    "DM_VACCINE.AGEU",
    "DM_VACCINE.ARM",
    "DM_VACCINE.DOMAIN",
    "DM_VACCINE.STUDYID",
    "DM_VACCINE.USUBJID"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADSL-1-read_data",
    "ADSL-init_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-filter_domain",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-filter_domain",
    "ADSL-PLANNED_ARM",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-PLANNED_ARM",
    "ADSL-ARM_GRP1",
    programs[["1_ADSL.R"]]
  )
  expect_section_order(
    "ADSL-ARM_GRP1",
    "ADSL-1-write_data",
    programs[["1_ADSL.R"]]
  )

  # Check generated ADSL
  x[[1]] |> source()
  expect_setequal(
    names(ADSL),
    c("ARM", "ARM_GRP1", "PLANNED_ARM", "USUBJID", "DOMAIN", "STUDYID", "AGEU")
  )
  expect_equal(nrow(ADSL), 306)
})

test_that("Global filter and domain filter incl. adsl dependencies", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adlb" = "column_dependencies_adlb_02.yml",
      "_mighty" = "_mighty_with_adsl_keys.yml"
    ),
    process_glue = FALSE
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("lb"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "LB.STUDYID",
    "LB.USUBJID",
    "LB.LBSEQ",
    "LB.VISITNUM",
    "LB.LBSTRESN",
    "LB.LBSTRESU",
    "ADSL.STUDYID",
    "ADSL.USUBJID",
    "ADSL.SEX"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "ADLB-1-read_data",
    "ADLB-init_domain",
    programs[["1_ADLB.R"]]
  )
  expect_section_order(
    "ADLB-init_domain",
    "ADLB-filter_domain",
    programs[["1_ADLB.R"]]
  )
  expect_section_order(
    "ADLB-filter_domain",
    "ADLB-1-write_data",
    programs[["1_ADLB.R"]]
  )
  # Check generated ADLB
  x[[1]] |> source()
  expect_setequal(
    names(ADLB),
    c("USUBJID", "STUDYID", "LBSEQ", "VISITNUM", "LBSTRESN", "LBSTRESU")
  )
  expect_equal(nrow(ADLB), 1790)
})


test_that("External predecessor dependencies are handled correctly in filter and col_echo", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adlb" = "column_dependencies_adlb_03.yml",
      "_mighty" = "_mighty_with_adsl_keys.yml"
    ),
    process_glue = FALSE
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("lb"),
    adam_domains = c("adsl")
  )
  standards_lib <- "mighty.standards"

  # ACT -------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    standards_lib = standards_lib,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "LB.STUDYID",
    "LB.USUBJID",
    "LB.LBSEQ",
    "LB.VISITNUM",
    "LB.LBSTRESN",
    "LB.LBSTRESU",
    "ADSL.STUDYID",
    "ADSL.USUBJID",
    "ADSL.SEX",
    "ADSL.AGE"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)

  expect_section_order(
    "ADLB-1-read_data",
    "ADLB-init_domain",
    programs[["1_ADLB.R"]]
  )
  expect_section_order(
    "ADLB-init_domain",
    "ADLB-filter_domain",
    programs[["1_ADLB.R"]]
  )
  expect_section_order("ADLB-filter_domain", "ADLB-AGE", programs[["1_ADLB.R"]])
  expect_section_order("ADLB-AGE", "ADLB-1-write_data", programs[["1_ADLB.R"]])

  # Check generated ADLB
  x[[1]] |> source()
  expect_setequal(
    names(ADLB),
    c("USUBJID", "STUDYID", "LBSEQ", "VISITNUM", "LBSTRESN", "LBSTRESU", "AGE")
  )
  expect_equal(nrow(ADLB), 1790)
})
