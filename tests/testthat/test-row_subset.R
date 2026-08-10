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
  dplyr::filter(LBTEST == \"Microcytes\") |>
  dplyr::mutate(LBTEST = \"Microcytes (new)\")
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
    subset: \"LBTEST == 'Microcytes'\"
" |>
    whisker::whisker.render(data = list(row_component = component_file))

  adam_specifications <- setup_study_dir(list("adlb" = yaml_content))

  study <- mighty.metadata::mighty_study(adam_specifications) |>
    mighty.metadata::resolve_subsets()

  # ACT -----------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = study,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_no_error()

  # EXPECT ----------------------------------------------------------------------

  row_action <- actual$program_sequence[
    outputs == "LBTEST" & type == "row_compute"
  ]

  expect_equal(nrow(row_action), 1)
  expect_equal(row_action$depend_cols[[1]]$domain, "ADLB")
  expect_equal(row_action$depend_cols[[1]]$column_name, "LBTEST")
  expect_equal(row_action$outputs[[1]], "LBTEST")
})
