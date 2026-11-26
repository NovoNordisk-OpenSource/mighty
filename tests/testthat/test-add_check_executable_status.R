local_fake_data_context <- function(tables_by_source) {
  list(
    get_datasource_names = function() names(tables_by_source),
    get_tables = function(datasource) {
      purrr::imap(tables_by_source[[datasource]], \(cols, name) {
        list(name = name, variables = stats::setNames(as.list(cols), cols))
      }) |>
        unname()
    }
  )
}

test_that("determine_available_columns labels tables by name, not datasource", {
  dc <- local_fake_data_context(list(
    sdtm = list(DM = c("USUBJID", "AGE")),
    adam = list(ADSL = "USUBJID"),
    md = list(MDPARAM = "PARAMCD"),
    other = list(CUSTOM = "X", `my-data` = "Y")
  ))

  actual <- determine_available_columns(dc)

  expect_equal(
    actual$domain_type,
    c("sdtm", "sdtm", "adam", "metadata", NA_character_, NA_character_)
  )
})

test_that("Metadata columns in the data context satisfy MD* dependencies", {
  dc <- local_fake_data_context(list(
    sdtm = list(LB = c("USUBJID", "LBTESTCD")),
    metadata = list(MDPARAM = c("PARAMCD", "LBTESTCD"))
  ))
  available_cols <- determine_available_columns(dc)
  depend_cols <- data.table::data.table(
    column_name = c("LBTESTCD", "PARAMCD"),
    domain = c("LB", "MDPARAM"),
    domain_type = classify_data_domains(c("LB", "MDPARAM"))
  )

  expect_identical(find_missing_dependencies(depend_cols, available_cols), NA)

  # A metadata column that is truly absent is still reported as missing
  depend_cols_missing <- data.table::data.table(
    column_name = "PARAM",
    domain = "MDPARAM",
    domain_type = "metadata"
  )
  actual <- find_missing_dependencies(depend_cols_missing, available_cols)[[1]]
  expect_equal(actual$column_name, "PARAM")
  expect_equal(actual$domain_type, "metadata")
})
