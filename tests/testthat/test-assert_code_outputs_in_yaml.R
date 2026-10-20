test_that("Error when code component outputs not referenced in YML", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
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
"

  comp <- "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(A=USUBJID) |>
  dplyr::mutate(B=USUBJID)

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

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})

test_that("Error when code component output has no overlap with YML", {
  # ARRANGE -----------------------------------------------------------------

  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
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
"

  comp <- "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL USUBJID
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(B=USUBJID)

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

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})

test_that("Error when a code component and a straight copy produce the same column", {
  # ARRANGE -----------------------------------------------------------------

  # B is produced twice: by ady.mustache and by the straight copy. The step that
  # runs second would overwrite the first, so the spec must be rejected.
  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
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
  - id: B
    method: ADSL.USUBJID
"

  comp <- "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(A=USUBJID) |>
  dplyr::mutate(B=USUBJID)

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

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})

test_that("Error when two code components produce the same column", {
  # ARRANGE -----------------------------------------------------------------

  # B is produced twice: by ady.mustache and by bdy.mustache. The step that runs
  # second would overwrite the first, so the spec must be rejected. bdy.mustache
  # is valid on its own: its @outputs match the YAML.
  yml <- "
id: ADSL
keys: [USUBJID]
label: foo
class: SUBJECT LEVEL ANALYSIS DATASET
structure: SUBJECT LEVEL ANALYSIS DATASET
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
  - id: B
    component:
      id: {{{bdy_custom}}}
"

  comp <- "
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL USUBJID
#' @outputs A
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(A=USUBJID) |>
  dplyr::mutate(B=USUBJID)

 " |>
    create_temp_component(filename = "ady.mustache")

  comp_b <- "
#' @title Second producer of B
#' @description
#' Produces B, which ady.mustache also produces.
#' @type column
#' @origin Derived
#' @method A method description
#' @depends ADSL USUBJID
#' @outputs B
#' @code
ADSL <- ADSL |>
  dplyr::mutate(B=USUBJID)

 " |>
    create_temp_component(filename = "bdy.mustache", dir = comp$dir)

  yaml_content <- whisker::whisker.render(
    yml,
    data = list(ady_custom = comp$filename, bdy_custom = comp_b$filename)
  )
  adam_specifications <- setup_study_dir(
    list("adsl" = yaml_content),
    extra_repos = comp$dir
  )

  # ACT & ASSERT ------------------------------------------------------------
  expect_snapshot_error(generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_temp_connector_config_path(),
    check_cross_domain_adam_dependencies = FALSE
  ))
})
