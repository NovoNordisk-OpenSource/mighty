# Unit tests ---------------------------------------------------------------

clash_copy <- function(column, domain = "ADLB") {
  data.table(
    node_id = paste0(domain, "-", column),
    code_id = NA_character_,
    type = "col_copy",
    depend_cols = list(
      data.table(column_name = column, domain = domain, domain_type = "adam")
    ),
    outputs = list(column),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = domain
  )
}

clash_compute <- function(column, code_id, depends, domain = "ADLB") {
  data.table(
    node_id = paste0(domain, "-", column),
    code_id = code_id,
    type = "col_compute",
    depend_cols = list(
      data.table(column_name = depends, domain = domain, domain_type = "adam")
    ),
    outputs = list(column),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = domain
  )
}

clash_keys <- list(
  LB = c("STUDYID", "USUBJID", "LBSEQ"),
  XL = c("STUDYID", "USUBJID", "LBSEQ"),
  SV = c("STUDYID", "USUBJID", "VISITNUM"),
  TV = c("STUDYID", "VISITNUM"),
  ADSL = c("STUDYID", "USUBJID"),
  ADLB = c("STUDYID", "USUBJID", "LBSEQ"),
  ADVS = c("STUDYID", "USUBJID", "VSSEQ")
)

# Runs the check through add_filter_domain_actions(), so the filter_domain
# outputs are built the same way as in generate_adam_code()
check_filter_clash <- function(
  actions,
  filter_depend_cols,
  filter_domain = list(ADLB = list(list(LB = NA_character_)))
) {
  add_filter_domain_actions(
    actions,
    filter_depend_cols,
    filter_domain,
    clash_keys
  )
}

adlb_base <- rbind(
  clash_copy("STUDYID"),
  clash_copy("USUBJID"),
  clash_copy("LBSEQ"),
  clash_copy("VISITNUM")
)

test_that("filter on another dataset's column errors when the column is copied", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_snapshot(
    check_filter_clash(actions, list(ADLB = "SV.VISIT")),
    error = TRUE
  )
})

test_that("filter on another dataset's column errors when a needed component builds it", {
  actions <- rbind(
    adlb_base,
    clash_compute("VISIT", "visit_label_01.R", "VISITNUM")
  )

  expect_snapshot(
    check_filter_clash(actions, list(ADLB = c("SV.VISIT", "VISIT"))),
    error = TRUE
  )
})

test_that("filter on another dataset's column errors when the component is needed through another component", {
  actions <- rbind(
    adlb_base,
    clash_compute("VISIT", "visit_label_01.R", "VISITNUM"),
    clash_compute("VISFL", "visit_flag_01.R", "VISIT")
  )

  expect_error(
    check_filter_clash(actions, list(ADLB = c("SV.VISIT", "VISFL"))),
    "visit_label_01.R"
  )
})

test_that("names that differ only in case clash", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_error(
    check_filter_clash(actions, list(ADLB = "sv.visit")),
    "Filter column name clash"
  )
})

test_that("dependencies that differ only in case are reported once", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_snapshot(
    check_filter_clash(actions, list(ADLB = c("SV.VISIT", "sv.visit"))),
    error = TRUE
  )
})

test_that("all clashes in all datasets are listed in one error", {
  actions <- rbind(
    adlb_base,
    clash_copy("VISIT"),
    clash_copy("SEX"),
    clash_copy("STUDYID", "ADVS"),
    clash_copy("USUBJID", "ADVS"),
    clash_copy("VSSEQ", "ADVS"),
    clash_copy("VISIT", "ADVS")
  )

  expect_snapshot(
    check_filter_clash(
      actions,
      list(ADLB = c("SV.VISIT", "ADSL.SEX"), ADVS = "SV.VISIT"),
      list(
        ADLB = list(list(LB = NA_character_)),
        ADVS = list(list(VS = NA_character_))
      )
    ),
    error = TRUE
  )
})

test_that("the same column joined in from two other datasets errors", {
  expect_snapshot(
    check_filter_clash(adlb_base, list(ADLB = c("SV.VISIT", "TV.VISIT"))),
    error = TRUE
  )
})

test_that("the same column from two other datasets errors when it is a key of only one", {
  # VISITNUM is a key of SV but not of ADSL, so the ADSL join adds a second
  # VISITNUM
  expect_error(
    check_filter_clash(
      adlb_base,
      list(ADLB = c("SV.VISITNUM", "ADSL.VISITNUM"))
    ),
    "joined in more than once"
  )
})

test_that("a clash without known starting datasets names 'the starting dataset'", {
  # A real spec always has a starting dataset, but internal callers and tests
  # can pass filter_domain without one
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_error(
    check_filter_clash(actions, list(ADLB = "SV.VISIT"), list(ADLB = NA)),
    "copied from the starting dataset"
  )
})

test_that("filter on another dataset's column doesn't clash when the dataset has no column with that name", {
  expect_no_error(check_filter_clash(adlb_base, list(ADLB = "SV.VISIT")))
})

test_that("column copied with method from the other dataset doesn't clash", {
  # col_echo actions run after the filter, so they are not in its outputs
  actions <- rbind(
    adlb_base,
    data.table(
      node_id = "ADLB-VISIT",
      code_id = NA_character_,
      type = "col_echo",
      depend_cols = list(
        data.table(column_name = "VISIT", domain = "SV", domain_type = "sdtm")
      ),
      outputs = list("VISIT"),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = "ADLB"
    )
  )

  expect_no_error(check_filter_clash(actions, list(ADLB = "SV.VISIT")))
})

test_that("column built by a component the filter doesn't need doesn't clash", {
  actions <- rbind(
    adlb_base,
    clash_compute("VISIT", "visit_label_01.R", "VISITNUM")
  )

  expect_no_error(check_filter_clash(actions, list(ADLB = "SV.VISIT")))
})

test_that("join keys of the other dataset don't clash", {
  expect_no_error(check_filter_clash(adlb_base, list(ADLB = "SV.VISITNUM")))
})

test_that("prefixes naming the dataset itself or its starting dataset are skipped", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_no_error(check_filter_clash(actions, list(ADLB = "ADLB.VISIT")))
  expect_no_error(check_filter_clash(actions, list(ADLB = "LB.VISIT")))
})

test_that("a prefix naming the second starting dataset is skipped", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_no_error(
    check_filter_clash(
      actions,
      list(ADLB = "XL.VISIT"),
      list(ADLB = list(list(LB = NA_character_), list(XL = NA_character_)))
    )
  )
})

test_that("a column built in another dataset doesn't clash", {
  # ADLB's OLDFL depends on ADSL.AGE2. The filter needs OLDFL, but ADLB itself
  # has no AGE2 column
  actions <- rbind(
    adlb_base,
    clash_copy("USUBJID", "ADSL"),
    clash_compute("AGE2", "age_crop_01.R", "AGE", "ADSL"),
    data.table(
      node_id = "ADLB-OLDFL",
      code_id = "oldfl_01.R",
      type = "col_compute",
      depend_cols = list(
        data.table(
          column_name = c("AGE2", "USUBJID"),
          domain = c("ADSL", "ADLB"),
          domain_type = "adam"
        )
      ),
      outputs = list("OLDFL"),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = "ADLB"
    )
  )

  expect_no_error(
    result <- check_filter_clash(actions, list(ADLB = c("ADSL.AGE2", "OLDFL")))
  )

  # ADSL's AGE2 action is not taken as a parent of OLDFL
  filter_outputs <- result[type == "filter_domain", outputs][[1]]
  expect_false("AGE2" %in% filter_outputs)
})

test_that("filters with only bare dependencies don't clash", {
  actions <- rbind(adlb_base, clash_copy("VISIT"))

  expect_no_error(check_filter_clash(actions, list(ADLB = c("VISIT", "LBSEQ"))))
})

test_that("bare dependency on a column built by a needed component doesn't clash", {
  actions <- rbind(
    adlb_base,
    clash_compute("VISIT", "visit_label_01.R", "VISITNUM")
  )

  expect_no_error(check_filter_clash(actions, list(ADLB = "VISIT")))
})

test_that("datasets without filter dependencies are skipped", {
  actions <- rbind(
    adlb_base,
    clash_copy("VISIT"),
    clash_copy("VISIT", "ADVS")
  )

  expect_no_error(
    check_filter_clash(
      actions,
      list(ADLB = "SV.VISITNUM", ADVS = NA_character_),
      list(
        ADLB = list(list(LB = NA_character_)),
        ADVS = list(list(VS = NA_character_))
      )
    )
  )
})

test_that("two other datasets sharing only a join key don't clash", {
  expect_no_error(
    check_filter_clash(adlb_base, list(ADLB = c("SV.USUBJID", "ADSL.USUBJID")))
  )
})

# End-to-end tests ---------------------------------------------------------

clash_mighty_yml <- "external_data:
  - id: LB
    keys: [STUDYID, USUBJID, LBSEQ]
  - id: SV
    keys: [STUDYID, USUBJID, VISITNUM]
repos:
  - 'github::NovoNordisk-OpenSource/mighty.standards/components'
  - 'fixtures/components'"

clash_adlb_yml <- function(population, columns) {
  paste0(
    "id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per lab test per visit
keys: [STUDYID, USUBJID, LBSEQ]
population:
",
    population,
    "columns:
  - id: STUDYID
  - id: USUBJID
  - id: LBSEQ
  - id: VISITNUM
",
    columns
  )
}

clash_global_population <- function(filter, depends) {
  paste0(
    "  base:
    - domain: LB
      filter: NA
      depends: [NA]
  global:
    - filter: ",
    filter,
    "
      depends: [",
    depends,
    "]
"
  )
}

generate_clash_study <- function(adlb_yml, path_connector_config) {
  adam_specifications <- setup_study_dir(list(
    "adlb" = adlb_yml,
    "_mighty" = clash_mighty_yml
  ))
  generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = FALSE
  )
}

# Generates the ADLB program, runs it, and returns the written ADLB
run_clash_study <- function(adlb_yml) {
  path <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path,
    sdtm_domains = c("lb", "sv")
  )
  actual <- generate_clash_study(adlb_yml, get_connector_config_path(path))
  write_adam_programs(dir = path, programs = actual$programs, style = FALSE)
  program <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  source(program[[1]], local = new.env())
  arrow::read_parquet(file.path(path, "data", "adam", "adlb.parquet"))
}

test_that("issue #86 spec errors: global filter on SV.VISIT with VISIT copied from LB", {
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    "  - id: VISIT\n"
  )

  expect_snapshot(
    generate_clash_study(adlb_yml, get_temp_connector_config_path()),
    error = TRUE
  )
})

test_that("global filter on SV.VISIT errors when the filter needs a component built from VISIT", {
  adlb_yml <- clash_adlb_yml(
    "  base:
    - domain: LB
      filter: NA
      depends: [NA]
  global:
    - filter: VISIT == 'BASELINE'
      depends: [SV.VISIT]
    - filter: VISFL == 'Y'
      depends: [VISFL]
",
    "  - id: VISIT
    component:
      id: visit_label_01.R
  - id: VISFL
    component:
      id: visit_flag_01.R
"
  )

  expect_snapshot(
    generate_clash_study(adlb_yml, get_temp_connector_config_path()),
    error = TRUE
  )
})

test_that("per-source filter on SV.VISIT errors with VISIT copied from LB", {
  adlb_yml <- clash_adlb_yml(
    "  base:
    - domain: LB
      filter: VISIT == 'BASELINE'
      depends: [SV.VISIT]
",
    "  - id: VISIT\n"
  )

  expect_error(
    generate_clash_study(adlb_yml, get_temp_connector_config_path()),
    "Filter column name clash"
  )
})

test_that("global filter on SV.VISIT runs with VISIT copied from SV by method", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    "  - id: VISIT\n    method: SV.VISIT\n"
  )

  adlb <- run_clash_study(adlb_yml)

  expect_gt(nrow(adlb), 0)
  expect_equal(unique(adlb$VISIT), "BASELINE")
})

test_that("global filter on SV.VISIT runs with VISIT built by a component the filter doesn't need", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    "  - id: VISIT\n    component:\n      id: visit_label_01.R\n"
  )

  adlb <- run_clash_study(adlb_yml)

  # Baseline is visit 3: only baseline rows are kept (the filter used SV's
  # VISIT), and VISIT holds the component's value
  expect_gt(nrow(adlb), 0)
  expect_equal(unique(adlb$VISIT), "VISIT 3")
})

test_that("global filter on SV.VISIT runs when ADLB has no VISIT column", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    ""
  )

  adlb <- run_clash_study(adlb_yml)

  expect_gt(nrow(adlb), 0)
  expect_false("VISIT" %in% names(adlb))
})

test_that("global filter on ADLB's own VISIT runs with VISIT copied from LB", {
  # The fix the error message suggests for the issue #86 spec
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "VISIT"),
    "  - id: VISIT\n"
  )

  adlb <- run_clash_study(adlb_yml)

  expect_gt(nrow(adlb), 0)
  expect_equal(unique(adlb$VISIT), "BASELINE")
})

test_that("global filter on a join key of SV runs", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISITNUM == 3", "SV.VISITNUM"),
    ""
  )

  adlb <- run_clash_study(adlb_yml)

  expect_gt(nrow(adlb), 0)
  expect_equal(unique(adlb$VISITNUM), 3)
})

test_that("global filter on SV.VISIT runs with ADLB's VISIT renamed to AVISIT", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    "  - id: AVISIT\n    method: ADLB.VISIT\n"
  )

  adlb <- run_clash_study(adlb_yml)

  # The rename runs before the filter, so ADLB has no VISIT when SV's is joined
  expect_gt(nrow(adlb), 0)
  expect_equal(unique(adlb$AVISIT), "BASELINE")
  expect_false("VISIT" %in% names(adlb))
})

test_that("global filter on SV.VISIT runs with VISIT renamed from another column (#107)", {
  skip_if_not_installed("arrow")
  adlb_yml <- clash_adlb_yml(
    clash_global_population("VISIT == 'BASELINE'", "SV.VISIT"),
    "  - id: VISIT\n    method: ADLB.LBTEST\n"
  )

  adlb <- run_clash_study(adlb_yml)

  # The rename runs after the filter, so ADLB has no VISIT when SV's is joined
  expect_gt(nrow(adlb), 0)
  expect_false("LBTEST" %in% names(adlb))
  expect_false(anyNA(adlb$VISIT))
})

test_that("global filter on SV.VISIT errors when the filter needs a col_mutate VISIT (#107)", {
  adlb_yml <- clash_adlb_yml(
    "  base:
    - domain: LB
      filter: NA
      depends: [NA]
  global:
    - filter: VISIT == 'BASELINE'
      depends: [SV.VISIT]
    - filter: VISFL == 'Y'
      depends: [VISFL]
",
    "  - id: LBTEST
  - id: VISIT
    method: ADLB.LBTEST
  - id: VISFL
    component:
      id: visit_flag_01.R
"
  )

  expect_snapshot(
    generate_clash_study(adlb_yml, get_temp_connector_config_path()),
    error = TRUE
  )
})
