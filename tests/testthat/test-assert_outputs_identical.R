test_that("Error is thrown when not all outputs in code component are referenced in YML", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
table_metadata:
  table: ADSL
init:
  base_domains:
    - DM
  filter_domain:
    - DM: NA
  filter_global: NA
  filter_depend_cols:
column_metadata:
  - column: USUBJID
  - column: A
    code_id: {{ady_custom}}
    
"

  tmp_file <- withr::local_tempdir() |>
    file.path("ady.mustache")
  "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type derivation
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
ADSL <- ADSL |> 
  dplyr::mutate(A=USUBJID) |> 
  dplyr::mutate(B=USUBJID)
}
 " |>
    writeLines(con = tmp_file)
  trial_path <- withr::local_tempdir()

  path_ui_data <- file.path(trial_path, "ui_yml.yml")
  yaml::read_yaml(
    text = whisker::whisker.render(yml, data = list(ady_custom = tmp_file))
  ) |>
    yaml::write_yaml(path_ui_data)
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  output_path <- trial_path
  # ACT & ASSERT ------------------------------------------------------------

  generate_adam_code(
    path_ui_data = path_ui_data,
    path_trial_metadata = path_trial_metadata,
    path_trial = trial_path,
    check_cross_domain_adam_dependencies = FALSE
  ) |>
    expect_error("Expected column outputs:")
})
