test_that("filter deps to multiple keyless domains error when cross-domain check disabled", {
  # SETUP -------------------------------------------------------------------

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

  adam_specifications <- setup_study_dir(list(
    "adlb" = yaml_content
  ))

  # ACT & EXPECT ------------------------------------------------------------
  expect_snapshot(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = get_temp_connector_config_path(),
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

# ADLB actions with col_copys USUBJID and LBSTRESN and a col_rename
# SRCSEQ <- LBSEQ, plus `extra` actions
rename_filter_actions <- function(extra = NULL) {
  actions <- data.table(
    node_id = c(
      "ADLB-col_copy-USUBJID",
      "ADLB-col_copy-LBSTRESN",
      "ADLB-col_rename-SRCSEQ"
    ),
    code_id = c(NA_character_, NA_character_, "mighty_col_rename"),
    type = c("col_copy", "col_copy", "col_rename"),
    depend_cols = list(
      data.table(column_name = "USUBJID", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "LBSTRESN", domain = "LB", domain_type = "sdtm"),
      data.table(column_name = "LBSEQ", domain = "ADLB", domain_type = "adam")
    ),
    outputs = list("USUBJID", "LBSTRESN", "SRCSEQ"),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )
  rbind(actions, extra)
}

extra_action <- function(
  type,
  output,
  depends,
  code_id = NA_character_,
  depends_domain = "ADLB"
) {
  data.table(
    node_id = paste0("ADLB-", type, "-", output),
    code_id = code_id,
    type = type,
    depend_cols = list(
      data.table(
        column_name = depends,
        domain = depends_domain,
        domain_type = "adam"
      )
    ),
    outputs = list(output),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = "ADLB"
  )
}

get_filter_outputs <- function(
  actions,
  filter_depends,
  filter_domain = list(ADLB = NA)
) {
  result <- add_filter_domain_actions(
    actions,
    filter_depend_cols = list(ADLB = filter_depends),
    filter_domain = filter_domain,
    domain_keys = list(ADLB = "USUBJID")
  )
  result[result$type == "filter_domain", ]$outputs[[1]]
}

test_that("Filter keeps the source column of a col_rename that runs after it", {
  outputs <- get_filter_outputs(rename_filter_actions(), "LBSTRESN")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "LBSEQ"))
})

test_that("Filter keeps the output of a col_rename it needs, not its source", {
  outputs <- get_filter_outputs(rename_filter_actions(), "SRCSEQ")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ"))
})

test_that("Filter keeps the outputs of a col_mutate it needs and of its parents", {
  actions <- rename_filter_actions(
    extra_action("col_mutate", "ZCOPY", "SRCSEQ", "mighty_col_mutate")
  )

  outputs <- get_filter_outputs(actions, "ZCOPY")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ", "ZCOPY"))
})

test_that("Filter keeps the parents of every column action it needs", {
  actions <- rename_filter_actions(rbind(
    data.table(
      node_id = "ADLB-col_rename-ATESTCD",
      code_id = "mighty_col_rename",
      type = "col_rename",
      depend_cols = list(
        data.table(
          column_name = "LBTESTCD",
          domain = "ADLB",
          domain_type = "adam"
        )
      ),
      outputs = list("ATESTCD"),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = "ADLB"
    ),
    extra_action("col_compute", "A", "SRCSEQ", "a.R"),
    extra_action("col_compute", "B", "ATESTCD", "b.R")
  ))

  outputs <- get_filter_outputs(actions, c("A", "B"))

  expect_setequal(
    outputs,
    c("USUBJID", "LBSTRESN", "SRCSEQ", "ATESTCD", "A", "B")
  )
})

test_that("A col_rename of a column action the filter needs runs after the filter", {
  actions <- rename_filter_actions(rbind(
    extra_action("col_compute", "X", "LBSTRESN", "x.R"),
    extra_action("col_rename", "Y", "X", "mighty_col_rename")
  ))

  outputs <- get_filter_outputs(actions, "X")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "LBSEQ", "X"))
})

test_that("Filter keeps the outputs of column actions that read the source of a col_rename it needs", {
  actions <- rename_filter_actions(rbind(
    extra_action("col_compute", "LBSEQX", "LBSEQ", "lbseqx.R"),
    extra_action("col_mutate", "LBSEQY", "LBSEQX", "mighty_col_mutate")
  ))

  outputs <- get_filter_outputs(actions, "SRCSEQ")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ", "LBSEQX"))
})

test_that("A col_rename whose source has the name of a joined column runs before the filter", {
  result <- add_filter_domain_actions(
    rename_filter_actions(),
    filter_depend_cols = list(ADLB = c("XL.LBSEQ", "XL.USUBJID")),
    filter_domain = list(ADLB = NA),
    domain_keys = list(ADLB = "USUBJID", XL = "USUBJID")
  )

  outputs <- result[result$type == "filter_domain", ]$outputs[[1]]
  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ"))
})

test_that("A col_rename whose source is a join key runs after the filter", {
  result <- add_filter_domain_actions(
    rename_filter_actions(),
    filter_depend_cols = list(ADLB = c("XL.LBSEQ", "XL.LBTEST")),
    filter_domain = list(ADLB = NA),
    domain_keys = list(ADLB = "USUBJID", XL = c("USUBJID", "LBSEQ"))
  )

  outputs <- result[result$type == "filter_domain", ]$outputs[[1]]
  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "LBSEQ"))
})

test_that("A col_rename whose output is a join key of a filter's joined domain runs before the filter", {
  result <- add_filter_domain_actions(
    rename_filter_actions(),
    filter_depend_cols = list(ADLB = "XL.LBTEST"),
    filter_domain = list(ADLB = NA),
    domain_keys = list(ADLB = "USUBJID", XL = c("USUBJID", "SRCSEQ"))
  )

  outputs <- result[result$type == "filter_domain", ]$outputs[[1]]
  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ"))
})

test_that("Joined filter columns leave out own and starting domain prefixes and join keys", {
  joined <- get_joined_filter_cols(
    filter_depends = c(
      "SV.VISITNUM",
      "SV.VISIT",
      "ADLB.AVAL",
      "LB.LBTEST",
      "SRCSEQ",
      NA
    ),
    target_domain = "ADLB",
    base_domains = "LB",
    domain_keys = list(SV = c("STUDYID", "USUBJID", "VISITNUM"))
  )

  expect_equal(joined, data.table(ref = "SV.VISIT", column = "VISIT"))
})

test_that("Joined filter columns are upper-cased and keep the dependency as written", {
  joined <- get_joined_filter_cols(
    filter_depends = c("sv.visit", "SV.VISIT"),
    target_domain = "ADLB",
    base_domains = "LB",
    domain_keys = list(SV = c("STUDYID", "USUBJID", "VISITNUM"))
  )

  expect_equal(
    joined,
    data.table(ref = c("sv.visit", "SV.VISIT"), column = "VISIT")
  )
})

test_that("Readers of the source of every col_rename that runs before the filter run before it", {
  # SRCSEQ is needed, Q reads LBSEQ so it runs first, Q's parent ATESTCD then
  # runs first too, and LBTCX reads ATESTCD's source LBTESTCD
  actions <- rename_filter_actions(rbind(
    extra_action("col_rename", "ATESTCD", "LBTESTCD", "mighty_col_rename"),
    extra_action("col_compute", "Q", c("LBSEQ", "ATESTCD"), "q.R"),
    extra_action("col_compute", "LBTCX", "LBTESTCD", "lbtcx.R")
  ))

  outputs <- get_filter_outputs(actions, "SRCSEQ")

  expect_setequal(
    outputs,
    c("USUBJID", "LBSTRESN", "SRCSEQ", "ATESTCD", "Q", "LBTCX")
  )
})

test_that("Error when a filter uses a col_rename source and joins a column with its name", {
  expect_snapshot(
    add_filter_domain_actions(
      rename_filter_actions(),
      filter_depend_cols = list(ADLB = c("XL.LBSEQ", "LBSEQ")),
      filter_domain = list(ADLB = NA),
      domain_keys = list(ADLB = "USUBJID", XL = "USUBJID")
    ),
    error = TRUE
  )
})

test_that("A col_mutate of a column (rename whose source is listed) runs after a filter on the source", {
  # The spec renames LBSEQ to SRCSEQ but also lists LBSEQ, so SRCSEQ is a col_mutate
  actions <- rbind(
    rename_filter_actions()[type == "col_copy"],
    extra_action("col_copy", "LBSEQ", "LBSEQ"),
    extra_action("col_mutate", "SRCSEQ", "LBSEQ", "mighty_col_mutate")
  )

  expect_setequal(
    get_filter_outputs(actions, "LBSEQ"),
    c("USUBJID", "LBSTRESN", "LBSEQ")
  )
  expect_setequal(
    get_filter_outputs(actions, "SRCSEQ"),
    c("USUBJID", "LBSTRESN", "LBSEQ", "SRCSEQ")
  )
})

test_that("Error when a filter uses a col_rename source and its output", {
  expect_snapshot(
    get_filter_outputs(rename_filter_actions(), c("LBSEQ", "SRCSEQ")),
    error = TRUE
  )
})

test_that("Error when a filter uses a col_rename source and a column made from its output", {
  actions <- rename_filter_actions(
    extra_action("col_compute", "SRCSEQ2", "SRCSEQ", "srcseq2_01.R")
  )

  expect_error(
    get_filter_outputs(actions, c("LBSEQ", "SRCSEQ2")),
    "renamed column under both names"
  )
})

test_that("A column action reading another dataset's column named like a col_rename source runs after the filter", {
  # OLDFL reads ADSL's LBSEQ, not the LBSEQ that SRCSEQ renames
  actions <- rename_filter_actions(
    extra_action("col_compute", "OLDFL", "LBSEQ", "oldfl.R", "ADSL")
  )

  outputs <- get_filter_outputs(actions, "SRCSEQ")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ"))
})

test_that("A column action reading a col_rename source through a starting dataset prefix runs before the filter", {
  # A component with `@depends LB LBSEQ` reads the same LBSEQ that SRCSEQ renames
  actions <- rename_filter_actions(
    extra_action("col_compute", "LBSEQX", "LBSEQ", "lbseqx.R", "LB")
  )

  outputs <- get_filter_outputs(
    actions,
    "SRCSEQ",
    filter_domain = list(ADLB = list(list(LB = NA_character_)))
  )

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ", "LBSEQX"))
})

test_that("Filter columns are matched to action columns regardless of case", {
  outputs <- get_filter_outputs(rename_filter_actions(), "srcseq")

  expect_setequal(outputs, c("USUBJID", "LBSTRESN", "SRCSEQ"))
})
