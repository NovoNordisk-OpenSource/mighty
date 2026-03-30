test_that("filter deps to multiple keyless domains error when cross-domain check disabled", {
  # SETUP -------------------------------------------------------------------
  trial_path <- withr::local_tempdir()

  # YAML spec that references both ADVS and ADAE domains in filter dependencies
  # Neither ADVS nor ADAE are present in _mighty.yml
  # No column external dependencies to avoid earlier validation errors
  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys:
  - USUBJID
  - PARAMCD

population:
  base:
    - domain: LB
      depends:
        - LBSTRESN
      filter: '!is.na(LBSTRESN)'
  global:
    - filter: '!is.na(AETERM)'
      depends:
        - ADAE.AETERM
    - filter: 'VSSEQ == 1'
      depends:
        - ADVS.VSSEQ

columns:
  - id: USUBJID
  - id: PARAMCD
  - id: LBSTRESN
"

  mighty_yml_content <- "keys: {}"

  adam_specifications <- setup_study_dir(list(
    "adlb" = yaml_content,
    "_mighty" = mighty_yml_content
  ))

  # ACT & EXPECT ------------------------------------------------------------
  expect_snapshot(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = trial_path,
      check_cross_domain_adam_dependencies = FALSE
    ),
    error = TRUE
  )
})


test_that("No filter_domain actions when no domains have filter dependencies", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = NA_character_)
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(ADLB = c("USUBJID", "PARAMCD"))

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  expect_equal(nrow(result), nrow(actions))
  expect_false("filter_domain" %in% result$type)
})

test_that("Self-referential filter dependency creates filter_domain action", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "PARAMCD")
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(ADLB = c("USUBJID", "PARAMCD"))

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  expect_equal(nrow(filter_actions), 1)
  expect_equal(filter_actions$domain, "ADLB")

  depend_cols <- filter_actions$depend_cols[[1]]
  paramcd_row <- depend_cols[depend_cols$column_name == "PARAMCD", ]
  expect_equal(nrow(paramcd_row), 1)
  expect_equal(paramcd_row$domain, "ADLB")
  expect_equal(paramcd_row$domain_type, "adam")
})

test_that("External domain dependency includes join keys", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "ADSL.SEX")
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(
    ADLB = c("USUBJID", "PARAMCD"),
    ADSL = "USUBJID"
  )

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  expect_equal(nrow(filter_actions), 1)

  depend_cols <- filter_actions$depend_cols[[1]]

  # Should include SEX from ADSL
  sex_row <- depend_cols[
    depend_cols$column_name == "SEX" & depend_cols$domain == "ADSL",
  ]
  expect_equal(nrow(sex_row), 1)

  # Should include USUBJID join key from ADSL
  key_row <- depend_cols[
    depend_cols$column_name == "USUBJID" & depend_cols$domain == "ADSL",
  ]
  expect_equal(nrow(key_row), 1)
})

test_that("Mixed self and external dependencies handled correctly", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = c("PARAMCD", "ADSL.SAFFL"))
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(
    ADLB = c("USUBJID", "PARAMCD"),
    ADSL = "USUBJID"
  )

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  depend_cols <- filter_actions$depend_cols[[1]]

  # Should have self-domain PARAMCD
  paramcd_self <- depend_cols[
    depend_cols$column_name == "PARAMCD" & depend_cols$domain == "ADLB",
  ]
  expect_equal(nrow(paramcd_self), 1)

  # Should have external SAFFL from ADSL
  saffl_ext <- depend_cols[
    depend_cols$column_name == "SAFFL" & depend_cols$domain == "ADSL",
  ]
  expect_equal(nrow(saffl_ext), 1)

  # Should have USUBJID join key from ADSL
  key_ext <- depend_cols[
    depend_cols$column_name == "USUBJID" & depend_cols$domain == "ADSL",
  ]
  expect_equal(nrow(key_ext), 1)
})

test_that("Multiple external domain dependencies include all join keys", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = c("ADSL.SEX", "EX.EXSTDY"))
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(
    ADLB = c("USUBJID", "PARAMCD"),
    ADSL = "USUBJID",
    EX = "USUBJID"
  )

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  depend_cols <- filter_actions$depend_cols[[1]]

  expect_true(any(
    depend_cols$column_name == "SEX" & depend_cols$domain == "ADSL"
  ))
  expect_true(any(
    depend_cols$column_name == "EXSTDY" & depend_cols$domain == "EX"
  ))
  expect_true(any(
    depend_cols$column_name == "USUBJID" & depend_cols$domain == "ADSL"
  ))
  expect_true(any(
    depend_cols$column_name == "USUBJID" & depend_cols$domain == "EX"
  ))
})

test_that("Multiple domains with filters each get filter_domain action", {
  actions <- data.table(
    node_id = c(
      "ADLB-col_copy-USUBJID",
      "ADLB-col_copy-PARAMCD",
      "ADVS-col_copy-USUBJID",
      "ADVS-col_copy-PARAMCD"
    ),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "USUBJID", domain = "VS", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "VS", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD", "USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = c("ADLB", "ADLB", "ADVS", "ADVS")
  )

  filter_depend_cols <- list(
    ADLB = "ADSL.SAFFL",
    ADVS = c("ADSL.SAFFL", "PARAMCD")
  )
  filter_domain <- list(ADLB = NA, ADVS = NA)
  domain_keys <- list(
    ADLB = c("USUBJID", "PARAMCD"),
    ADVS = c("USUBJID", "PARAMCD"),
    ADSL = "USUBJID"
  )

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  expect_equal(nrow(filter_actions), 2)
  expect_setequal(filter_actions$domain, c("ADLB", "ADVS"))
})

test_that("Filter dependency on col_compute includes compute output", {
  actions <- data.table(
    node_id = c(
      "ADLB-col_copy-USUBJID",
      "ADLB-col_copy-PARAMCD",
      "ADLB-col_compute-SAFFL"
    ),
    code_id = c(NA_character_, NA_character_, "der_saffl.mustache"),
    type = c("col_copy", "col_copy", "col_compute"),
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "USUBJID", domain = "ADLB", domain_type = "adam")
    ),
    outputs = list("USUBJID", "PARAMCD", "SAFFL"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "SAFFL")
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(ADLB = c("USUBJID", "PARAMCD"))

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  outputs <- filter_actions$outputs[[1]]

  expect_true("SAFFL" %in% outputs)
})

test_that("External domain with multi-column keys includes all keys", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "MDPARAM.PARAMVAL")
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(
    ADLB = c("USUBJID", "PARAMCD"),
    MDPARAM = c("STUDYID", "TOPICCD")
  )

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  depend_cols <- filter_actions$depend_cols[[1]]

  expect_true(any(
    depend_cols$column_name == "STUDYID" & depend_cols$domain == "MDPARAM"
  ))
  expect_true(any(
    depend_cols$column_name == "TOPICCD" & depend_cols$domain == "MDPARAM"
  ))
})

test_that("Domain filter present adds SRC_ to depend_cols", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "PARAMCD")
  filter_domain <- list(ADLB = list(list(filter = "!is.na(X)", depends = "X")))
  domain_keys <- list(ADLB = c("USUBJID", "PARAMCD"))

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  depend_cols <- filter_actions$depend_cols[[1]]

  expect_true("SRC_" %in% depend_cols$column_name)
})

test_that("Multiple domains with col_copy filter dependencies", {
  # Verifies each domain's filter_domain gets only its own col_copy outputs.
  # Catches bug where dplyr filter with .data pronoun caused cross-domain contamination.

  actions <- data.table(
    node_id = c("ADSL-col_copy-DOMAIN", "ADLB-col_copy-LBSEQ"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "DOMAIN", domain = "DM", domain_type = "sdtm"),
      data.table(column_name = "LBSEQ", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("DOMAIN", "LBSEQ"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = c("ADSL", "ADLB")
  )

  filter_depend_cols <- list(ADSL = "DOMAIN", ADLB = "LBSEQ")
  filter_domain <- list(ADSL = NA, ADLB = NA)
  domain_keys <- list(ADSL = "USUBJID", ADLB = "USUBJID")

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  adsl_filter <- result[type == "filter_domain" & domain == "ADSL"]
  adlb_filter <- result[type == "filter_domain" & domain == "ADLB"]

  # Each domain should only have its own col_copy output
  expect_true("DOMAIN" %in% adsl_filter$outputs[[1]])
  expect_false("LBSEQ" %in% adsl_filter$outputs[[1]])

  expect_true("LBSEQ" %in% adlb_filter$outputs[[1]])
  expect_false("DOMAIN" %in% adlb_filter$outputs[[1]])
})

test_that("Multiple domains with col_compute filter dependencies", {
  # Minimal test: two domains each with a col_compute that matches its filter dependency.
  # Verifies each domain's filter_domain gets only its own col_compute output.

  actions <- data.table(
    node_id = c("ADSL-col_compute-FLAG1", "ADLB-col_compute-FLAG2"),
    code_id = c("der_flag1.mustache", "der_flag2.mustache"),
    type = "col_compute",
    depend_cols = list(
      data.table(column_name = "X", domain = "DM", domain_type = "sdtm"),
      data.table(column_name = "Y", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("FLAG1", "FLAG2"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = c("ADSL", "ADLB")
  )

  filter_depend_cols <- list(ADSL = "FLAG1", ADLB = "FLAG2")
  filter_domain <- list(ADSL = NA, ADLB = NA)
  domain_keys <- list(ADSL = "USUBJID", ADLB = "USUBJID")

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  adsl_filter <- result[type == "filter_domain" & domain == "ADSL"]
  adlb_filter <- result[type == "filter_domain" & domain == "ADLB"]

  # Each domain should only have its own col_compute output
  expect_true("FLAG1" %in% adsl_filter$outputs[[1]])
  expect_false("FLAG2" %in% adsl_filter$outputs[[1]])

  expect_true("FLAG2" %in% adlb_filter$outputs[[1]])
  expect_false("FLAG1" %in% adlb_filter$outputs[[1]])
})

test_that("Domain filter absent does not add SRC_ to depend_cols", {
  actions <- data.table(
    node_id = c("ADLB-col_copy-USUBJID", "ADLB-col_copy-PARAMCD"),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "PARAMCD", domain = "LB", domain_type = "sdtm")
    ),
    outputs = list("USUBJID", "PARAMCD"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )

  filter_depend_cols <- list(ADLB = "PARAMCD")
  filter_domain <- list(ADLB = NA)
  domain_keys <- list(ADLB = c("USUBJID", "PARAMCD"))

  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  filter_actions <- result[result$type == "filter_domain", ]
  depend_cols <- filter_actions$depend_cols[[1]]

  expect_false("SRC_" %in% depend_cols$column_name)
})
