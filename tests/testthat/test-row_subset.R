test_that("subsetted row component keeps clean @depends/@outputs (mighty.metadata#44)", {
  # ARRANGE -------------------------------------------------------------------

  component_file <- withr::local_tempdir() |>
    file.path("new_lbtest_subset.mustache")
  "
#' @title New lbtest subset
#' @description A description
#' @param domain `character` Name of the domain being derived
#' @type row
#' @depends {{{domain}}} LBTEST
#' @outputs LBTEST
#' @code
new_lbtest <- {{{domain}}} |>

  dplyr::filter(LBTEST == \"Albumin\") |>
  dplyr::mutate(LBTEST = \"Albumin (new)\")
{{{domain}}} <- rbind({{{domain}}}, new_lbtest)
" |>
    writeLines(con = component_file)

  yaml_content <- "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]

population:
  base:
    - domain: LB
      filter: NA
      depends:
        - NA
  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID
  - id: LBTEST

rows:
  - id: NEW_LBTEST_SUBSET
    component:
      id: {{{row_component}}}
      with:
        domain: ADLB
    subset: \"USUBJID == '01-708-1216'\"
" |>
    whisker::whisker.render(data = list(row_component = component_file))

  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  study <- mighty.metadata::mighty_study(adam_specifications) |>
    mighty.metadata::resolve_subsets()

  path_connector_config <- withr::local_tempdir()
  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = "lb"
  )

  # ACT -----------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = study,
    path_connector_config = get_connector_config_path(path_connector_config),
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  # EXPECT: @depends/@outputs stayed clean -------------------------------------

  row_action <- actual$program_sequence[
    outputs == "LBTEST" & type == "row_compute"
  ]

  expect_equal(nrow(row_action), 1)
  expect_equal(row_action$depend_cols[[1]]$domain, "ADLB")
  expect_equal(row_action$depend_cols[[1]]$column_name, "LBTEST")
  expect_equal(row_action$outputs[[1]], "LBTEST")

  # EXPECT: row operation ran correctly on real data ---------------------------

  lb <- pharmaversesdtm::lb
  target_subject <- "01-708-1216"
  n_lb_albumin <- sum(lb$LBTEST == "Albumin")
  n_target_albumin <- sum(lb$USUBJID == target_subject & lb$LBTEST == "Albumin")

  actual$programs[1][[1]] <- remove_connector_write_step(actual$programs[1][[
    1
  ]])
  write_adam_programs(actual$programs, path_connector_config, style = FALSE)
  program <- list.files(
    path_connector_config,
    pattern = "\\.R$",
    full.names = TRUE
  )

  expect_no_error(source(program))

  # Original rows are untouched, and one new row is appended per subsetted row
  # that matches the component's filter (only the target subject's Albumin rows)
  expect_equal(nrow(ADLB), nrow(lb) + n_target_albumin)
  expect_equal(sum(ADLB$LBTEST == "Albumin"), n_lb_albumin)
  expect_equal(sum(ADLB$LBTEST == "Albumin (new)"), n_target_albumin)

  # Rows for every other subject are untouched by the subset. Row order isn't
  # preserved through the generated program, so compare content order-independently.
  other_subjects <- ADLB[ADLB$USUBJID != target_subject, c("USUBJID", "LBTEST")]
  other_subjects <- other_subjects[
    order(other_subjects$USUBJID, other_subjects$LBTEST),
  ]
  expected_other <- lb[lb$USUBJID != target_subject, c("USUBJID", "LBTEST")]
  expected_other <- expected_other[
    order(expected_other$USUBJID, expected_other$LBTEST),
  ]
  expect_equal(other_subjects, expected_other, ignore_attr = TRUE)
})
