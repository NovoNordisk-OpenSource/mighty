test_that("col_rename generated ADLB program executes without error", {
  # SETUP -------------------------------------------------------------------

  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adlb" = "col_rename_adlb_01.yml")
  )
  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = "lb"
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(
    dir = path_connector_config,
    programs = actual$programs,
    style = TRUE
  )

  # EXPECT ------------------------------------------------------------------

  x <- list.files(path_connector_config, pattern = "\\.R$", full.names = TRUE)
  expect_no_error(source(x[[1]]))
})

# Generates the ADLB program from a study dir, runs it on pharmaverse LB and
# returns the written ADLB dataset
run_adlb_program <- function(adam_specifications) {
  path_connector_config <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = "lb"
  )

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  )
  write_adam_programs(
    dir = path_connector_config,
    programs = actual$programs,
    style = FALSE
  )

  program <- list.files(
    path_connector_config,
    pattern = "\\.R$",
    full.names = TRUE
  )
  source(program[[1]], local = new.env())
  arrow::read_parquet(file.path(
    path_connector_config,
    "data",
    "adam",
    "adlb.parquet"
  ))
}

adlb_rename_yaml <- function(population, columns) {
  paste0(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID, STUDYID]
population:
",
    population,
    "
columns:
  - id: USUBJID
  - id: STUDYID
",
    columns
  )
}

test_that("col_rename is kept when the dataset has a base filter", {
  adam_specifications <- setup_study_from_fixtures(
    fixtures = list("adlb" = "col_rename_adlb_03.yml")
  )

  adlb <- run_adlb_program(adam_specifications)

  expect_named(adlb, c("USUBJID", "STUDYID", "SRCSEQ", "LBSTRESN", "AVAL"))
  expect_false(anyNA(adlb$LBSTRESN))
  expect_false(anyNA(adlb$SRCSEQ))
})

test_that("col_renames and the steps that read them run after a global filter", {
  yaml_content <- adlb_rename_yaml(
    population = "
  base:
    - {domain: LB, depends: [NA], filter: NA}
  global:
    - {filter: '!is.na(LBSTRESN)', depends: [LBSTRESN]}
",
    columns = "
  - {id: SRCSEQ, method: LB.LBSEQ}
  - {id: ATESTCD, method: ADLB.LBTESTCD}
  - {id: ZCOPY, method: ADLB.SRCSEQ}
  - {id: SRCSEQ2, component: {id: srcseq2_01.R}}
  - {id: LBSTRESN}
"
  )
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  adlb <- run_adlb_program(adam_specifications)

  expect_named(
    adlb,
    c("USUBJID", "STUDYID", "SRCSEQ", "ATESTCD", "ZCOPY", "SRCSEQ2", "LBSTRESN")
  )
  expect_false(anyNA(adlb$LBSTRESN))
  expect_equal(adlb$ZCOPY, adlb$SRCSEQ)
  expect_equal(adlb$SRCSEQ2, adlb$SRCSEQ * 2)
})

test_that("a filter can use a renamed column, a mutate of it and a step reading its source", {
  yaml_content <- adlb_rename_yaml(
    population = "
  base:
    - {domain: LB, depends: [NA], filter: NA}
  global:
    - {filter: 'SRCSEQ > 1 & ZCOPY < 100', depends: [SRCSEQ, ZCOPY]}
",
    columns = "
  - {id: SRCSEQ, method: ADLB.LBSEQ}
  - {id: ZCOPY, method: ADLB.SRCSEQ}
  - {id: ATESTCD, method: ADLB.LBTESTCD}
  - {id: LBSEQX, component: {id: lbseqx_01.R}}
  - {id: LBSTRESN}
"
  )
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  adlb <- run_adlb_program(adam_specifications)

  expect_named(
    adlb,
    c("USUBJID", "STUDYID", "SRCSEQ", "ZCOPY", "ATESTCD", "LBSEQX", "LBSTRESN")
  )
  expect_equal(min(adlb$SRCSEQ), 2)
  expect_lt(max(adlb$ZCOPY), 100)
  expect_false(anyNA(adlb$ATESTCD))
  expect_equal(adlb$LBSEQX, adlb$SRCSEQ * 10)
})

test_that("a filter can use the source column of a col_rename", {
  yaml_content <- adlb_rename_yaml(
    population = "
  base:
    - {domain: LB, depends: [LBSEQ], filter: 'LBSEQ > 1'}
  global:
    - {filter: 'LBSEQ < 100', depends: [LBSEQ]}
",
    columns = "
  - {id: SRCSEQ, method: ADLB.LBSEQ}
  - {id: LBSTRESN}
"
  )
  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  adlb <- run_adlb_program(adam_specifications)

  expect_named(adlb, c("USUBJID", "STUDYID", "SRCSEQ", "LBSTRESN"))
  expect_equal(min(adlb$SRCSEQ), 2)
  expect_lt(max(adlb$SRCSEQ), 100)
})
