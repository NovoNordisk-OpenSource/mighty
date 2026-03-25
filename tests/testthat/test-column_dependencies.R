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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check outputs of read_data
  expected_outputs <- c(
    "dm.STUDYID",
    "dm.USUBJID",
    "dm.ARM",
    "dm_vaccine.STUDYID",
    "dm_vaccine.USUBJID",
    "dm_vaccine.ARM"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check outputs of read_data
  expected_outputs <- c(
    "dm.USUBJID",
    "dm.STUDYID",
    "dm.ARM",
    "dm.ACTARM",
    "dm_vaccine.USUBJID",
    "dm_vaccine.STUDYID",
    "dm_vaccine.ARM",
    "dm_vaccine.ACTARM"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "dm.USUBJID",
    "dm.DOMAIN",
    "dm.STUDYID",
    "dm.ARM",
    "dm_vaccine.USUBJID",
    "dm_vaccine.DOMAIN",
    "dm_vaccine.STUDYID",
    "dm_vaccine.ARM"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "dm.STUDYID",
    "dm.USUBJID",
    "dm.DTHFL",
    "dm.ARM",
    "dm_vaccine.STUDYID",
    "dm_vaccine.USUBJID",
    "dm_vaccine.DTHFL",
    "dm_vaccine.ARM"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "dm.STUDYID",
    "dm.USUBJID",
    "dm.ARM",
    "dm_vaccine.STUDYID",
    "dm_vaccine.USUBJID",
    "dm_vaccine.ARM"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "dm.AGEU",
    "dm.ARM",
    "dm.DOMAIN",
    "dm.STUDYID",
    "dm.USUBJID",
    "dm_vaccine.AGEU",
    "dm_vaccine.ARM",
    "dm_vaccine.DOMAIN",
    "dm_vaccine.STUDYID",
    "dm_vaccine.USUBJID"
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

test_that("Global filter and domain filter incl. adsl dependencies (lower case)", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adlb" = "column_dependencies_adlb_02.yml",
      "_mighty" = "_mighty_lowercase_adsl.yml"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "lb.STUDYID",
    "lb.USUBJID",
    "lb.LBSEQ",
    "lb.VISITNUM",
    "lb.LBSTRESN",
    "lb.LBSTRESU",
    "adsl.STUDYID",
    "adsl.USUBJID",
    "adsl.SEX"
  )
  expect_setequal(actual$program_sequence$outputs[[1]], expected_ext_dep)

  # Check order of derivations
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  expect_section_order(
    "adlb-1-read_data",
    "adlb-init_domain",
    programs[["1_adlb.R"]]
  )
  expect_section_order(
    "adlb-init_domain",
    "adlb-filter_domain",
    programs[["1_adlb.R"]]
  )
  expect_section_order(
    "adlb-filter_domain",
    "adlb-1-write_data",
    programs[["1_adlb.R"]]
  )
  # Check generated ADLB
  x[[1]] |> source()
  expect_setequal(
    names(adlb),
    c("USUBJID", "STUDYID", "LBSEQ", "VISITNUM", "LBSTRESN", "LBSTRESU")
  )
  expect_equal(nrow(adlb), 1790)
})


test_that("External predecessor dependencies are handled correctly in filter and col_echo", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adlb" = "column_dependencies_adlb_03.yml",
      "_mighty" = "_mighty_lowercase_adsl.yml"
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

    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(dir = path_connector_config, programs = actual$programs)
  x <- list.files(path_connector_config, pattern = ".R", full.names = TRUE)

  # EXPECT -------------------------------------------------------------------

  # Check external dependencies
  expected_ext_dep <- c(
    "lb.STUDYID",
    "lb.USUBJID",
    "lb.LBSEQ",
    "lb.VISITNUM",
    "lb.LBSTRESN",
    "lb.LBSTRESU",
    "adsl.STUDYID",
    "adsl.USUBJID",
    "adsl.SEX",
    "adsl.AGE"
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
