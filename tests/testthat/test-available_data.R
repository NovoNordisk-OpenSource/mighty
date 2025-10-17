test_that("data_context provides metadata from sdtm.dm testdata", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = output_path,
    sdtm_domains = c("dm")
  )
  cnt <- connector::connect(file.path(output_path, "_connector.yml"))

  # ACT ---------------------------------------------------------------------
  dc <- data_context$new(cnt)

  # EXPECT ------------------------------------------------------------------

  # Test default table names (datasource = "sdtm")
  expect_equal(names(dc$get_table_names()), c("sdtm.dm"))
  expect_equal(dc$get_table_names()[[1]], "dm")

  dm_columns <- c(
    "STUDYID",
    "DOMAIN",
    "USUBJID",
    "SUBJID",
    "RFSTDTC",
    "RFENDTC",
    "RFXSTDTC",
    "RFXENDTC",
    "RFICDTC",
    "RFPENDTC",
    "DTHDTC",
    "DTHFL",
    "SITEID",
    "AGE",
    "AGEU",
    "SEX",
    "RACE",
    "ETHNIC",
    "ARMCD",
    "ARM",
    "ACTARMCD",
    "ACTARM",
    "COUNTRY",
    "DMDTC",
    "DMDY"
  )
  expect_true(all(
    names(dc$get_table_variables(table_name = "dm")) %in% dm_columns
  ))
  expect_false(dc$has_variables(table_name = "dm", variable_name = "TOPICCD"))
  # Test variable from non-existing data set
  expect_false(dc$has_variables(
    table_name = "dm_vaccine",
    variable_name = "USUBJID"
  ))
})

test_that("data_context provides metadata from sdtm and adam", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    adam_domains = c("adsl"),
    test_data_path = output_path
  )
  # replicate DM.SV dataset to METADATA.MDVISIT to be able to query metadata
  file.copy(
    file.path(output_path, "data", "sdtm", "sv.parquet"),
    file.path(output_path, "data", "metadata", "mdvisit.parquet")
  )

  cnt <- connector::connect(file.path(output_path, "_connector.yml"))

  # ACT ---------------------------------------------------------------------

  dc <- data_context$new(cnt)

  adamdata <- dc$get_tables(datasources = c("adam"))
  metadata <- dc$get_tables(datasources = c("metadata"))
  sdtmdata <- dc$get_tables()
  # EXPECT ------------------------------------------------------------------

  # Test default table names (datasource = "sdtm")
  expect_equal(
    sort(names(dc$get_table_names())),
    c(
      "sdtm.ae",
      "sdtm.dm",
      "sdtm.dm_vaccine",
      "sdtm.lb",
      "sdtm.suppdm",
      "sdtm.sv"
    )
  )
  expect_equal(
    sort(as.character(dc$get_table_names())),
    c("ae", "dm", "dm_vaccine", "lb", "suppdm", "sv")
  )

  # Test table names in both SDTM and ADAM
  expect_equal(
    sort(names(dc$get_table_names(c("sdtm", "adam")))),
    c(
      "adam.adsl",
      "sdtm.ae",
      "sdtm.dm",
      "sdtm.dm_vaccine",
      "sdtm.lb",
      "sdtm.suppdm",
      "sdtm.sv"
    )
  )
  expect_equal(
    sort(as.character(dc$get_table_names(c("sdtm", "adam")))),
    c("adsl", "ae", "dm", "dm_vaccine", "lb", "suppdm", "sv")
  )

  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = TRUE)[[1]],
    c("adam.adsl")
  )
  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = TRUE)[[
      "adam.adsl"
    ]],
    c("adam.adsl")
  )
  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = FALSE)[[
      "adam.adsl"
    ]],
    c("adsl")
  )

  expect_true(dc$has_variables(
    table_name = "dm",
    variable_name = c("STUDYID", "USUBJID")
  ))
  expect_false(dc$has_variables(
    table_name = "dm",
    variable_name = "NONEXISTING"
  ))
  expect_true(dc$has_variables(
    datasource = "metadata",
    table_name = "mdvisit",
    variable_name = "STUDYID"
  ))

  expect_equal(
    dc$get_table_variables(table_name = "sv"),
    dc$get_table_variables(datasource = "metadata", table_name = "mdvisit")
  )
})

test_that("data_context provide column metadata", {
  # SETUP -------------------------------------------------------------------
  output_path <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    sdtm_domains = ("dm"),
    adam_domains = c("adsl"),
    test_data_path = output_path
  )
  cnt <- connector::connect(file.path(output_path, "_connector.yml"))

  # ACT ---------------------------------------------------------------------
  dc <- data_context$new(cnt)
  # EXPECT ------------------------------------------------------------------

  expect_equal(dc$get_variable_type("adsl", "USUBJID", "adam"), "character")
  expect_equal(dc$get_variable_type("adsl", "AGE", "adam"), "numeric")

  expect_equal(
    dc$get_variable_label("dm", "USUBJID"),
    "Unique Subject Identifier"
  )
  expect_equal(
    dc$get_variable_label("adsl", "USUBJID", "adam"),
    dc$get_variable_label("dm", "USUBJID")
  )
})
