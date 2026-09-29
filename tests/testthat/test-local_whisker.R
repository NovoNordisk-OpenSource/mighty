test_that("Depends parameters replaced with actual user-supplied values", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID]
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
      id: {{{ady_custom}}}
      with:
        depends_var: 'USUBJID'
        output_var: 'A'
"

  comp <- "
#' @title Analysis relative day
#' @description desc
#' Derives the relative day compared to the treatment start date.
#'
#' @param depends_var depends variable
#' @param output_var output variable
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL {{{depends_var}}}
#' @outputs {{{output_var}}}
#' @code
{{{output_var}}} <- {{{output_var}}} |>
  dplyr::mutate(U2={{{depends_var}}})
 " |>
    create_temp_component(filename = "ady.mustache")

  yaml_content <- whisker::whisker.render(
    yml,
    data = list(ady_custom = comp$filename)
  )
  adam_specifications <- setup_study_dir(
    list("adsl" = yaml_content),
    extra_repos = comp$dir
  )

  # ACT ------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
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
