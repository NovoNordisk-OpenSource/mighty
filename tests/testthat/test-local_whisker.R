test_that("Depends parameters replaced with actual user-supplied values", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
keys: []
population:
  base:
    - domain: DM
      depends:
        - NA
      filter: NA
  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID
  - id: A
    component:
      id: {{ady_custom}}
      with:
        depends_var: 'USUBJID'
        output_var: 'A'
"

  tmp_file <- withr::local_tempdir() |>
    file.path("ady.mustache")
  "
#' @title Analysis relative day
#' @description desc
#' Derives the relative day compared to the treatment start date.
#'
#' @param depends_var depends variable
#' @param output_var output variable
#' @type derivation
#' @depends ADSL {{depends_var}}
#' @outputs {{output_var}}
#' @code
{{output_var}} <- {{output_var}} |>
  dplyr::mutate(U2={{depends_var}})
 " |>
    writeLines(con = tmp_file)
  trial_path <- withr::local_tempdir()

  adam_specifications <- file.path(trial_path, "ui_yml.yml")
  whisker::whisker.render(yml, data = list(ady_custom = tmp_file)) |>
    writeLines(adam_specifications)

  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  output_path <- trial_path
  # ACT ------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_trial_metadata = path_trial_metadata,
    path_trial = trial_path,
    check_cross_domain_adam_dependencies = FALSE
  )

  # ASSERT -----------------------------------------------------------------
  #
  actual$program_sequence[outputs == "A", code] |>
    strsplit("\n") |>
    _[[1]][3:4] |>
    expect_snapshot()

  actual_code <- actual$program_sequence[outputs == "A", code] |>
    strsplit("\n") |>
    _[[1]][3:4]
  expect_snapshot(actual_code)
})
