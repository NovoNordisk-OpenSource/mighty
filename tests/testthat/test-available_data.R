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
  expect_equal(names(dc$get_table_names()), c("sdtm.DM"))
  expect_equal(dc$get_table_names()[[1]], "DM")

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
    dm_columns %in% names(dc$get_table_variables(table_name = "DM"))
  ))
  expect_false(dc$has_variables(table_name = "DM", variable_name = "TOPICCD"))
  # Test variable from non-existing data set
  expect_false(dc$has_variables(
    table_name = "DM_VACCINE",
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
      "sdtm.AE",
      "sdtm.DM",
      "sdtm.DM_VACCINE",
      "sdtm.LB",
      "sdtm.SUPPDM",
      "sdtm.SV"
    )
  )
  expect_equal(
    sort(as.character(dc$get_table_names())),
    c("AE", "DM", "DM_VACCINE", "LB", "SUPPDM", "SV")
  )

  # Test table names in both SDTM and ADAM
  expect_equal(
    sort(names(dc$get_table_names(c("sdtm", "adam")))),
    c(
      "adam.ADSL",
      "sdtm.AE",
      "sdtm.DM",
      "sdtm.DM_VACCINE",
      "sdtm.LB",
      "sdtm.SUPPDM",
      "sdtm.SV"
    )
  )
  expect_equal(
    sort(as.character(dc$get_table_names(c("sdtm", "adam")))),
    c("ADSL", "AE", "DM", "DM_VACCINE", "LB", "SUPPDM", "SV")
  )

  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = TRUE)[[1]],
    c("adam.ADSL")
  )
  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = TRUE)[[
      "adam.ADSL"
    ]],
    c("adam.ADSL")
  )
  expect_equal(
    dc$get_table_names(datasources = c("adam"), prefix_datasource = FALSE)[[
      "adam.ADSL"
    ]],
    c("ADSL")
  )

  expect_true(dc$has_variables(
    table_name = "DM",
    variable_name = c("STUDYID", "USUBJID")
  ))
  expect_false(dc$has_variables(
    table_name = "DM",
    variable_name = "NONEXISTING"
  ))
  expect_true(dc$has_variables(
    datasource = "metadata",
    table_name = "MDVISIT",
    variable_name = "STUDYID"
  ))

  expect_equal(
    dc$get_table_variables(table_name = "SV"),
    dc$get_table_variables(datasource = "metadata", table_name = "MDVISIT")
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

  expect_equal(dc$get_variable_type("ADSL", "USUBJID", "adam"), "character")
  expect_equal(dc$get_variable_type("ADSL", "AGE", "adam"), "numeric")

  expect_equal(
    dc$get_variable_label("DM", "USUBJID"),
    "Unique Subject Identifier"
  )
  expect_equal(
    dc$get_variable_label("ADSL", "USUBJID", "adam"),
    dc$get_variable_label("DM", "USUBJID")
  )
})
