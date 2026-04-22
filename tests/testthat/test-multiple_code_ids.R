test_that("Multiple columns using same code_id, but diff parameters errors out", {
  component_file <- withr::local_tempfile(fileext = ".mustache")
  writeLines(
    "
#' @title fn_AB
#' @description Multi-output parameterized test component
#' @param param_1 `character` A test parameter
#' @param param_2 `character` A test parameter
#' @type derivation
#' @depends ADLB USUBJID
#' @outputs A
#' @outputs B
#' @code
ADLB <- ADLB |>
  dplyr::mutate(A = USUBJID, B = USUBJID)
",
    con = component_file
  )

  yml <- whisker::whisker.render(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
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
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
  - id: B
    component:
      id: {{component_file}}
      with:
        param_1: x
        param_2: y
",
    data = list(component_file = component_file)
  )

  adam_specifications <- setup_study_dir(list(
    "adlb" = yml
  ))
  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      check_cross_domain_adam_dependencies = FALSE,
      path_connector_config = get_temp_connector_config_path()
    ),
    regexp = "different parameter values"
  )
})

test_that("Same component with different parameters and disjoint outputs succeeds", {
  component_file <- withr::local_tempfile(fileext = ".mustache")
  writeLines(
    "
#' @title param_1_new_1_val
#' @description Assigns a new variable with name and value from parameters.
#' @param value `character` The value to assign
#' @param new_variable `character` Name of the new variable
#' @type derivation
#' @outputs {{new_variable}}
#' @code
ADLB <- ADLB |>
  dplyr::mutate({{new_variable}} = {{value}})
",
    con = component_file
  )

  yml <- whisker::whisker.render(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
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
      id: {{component_file}}
      with:
        new_variable: A
        value: '1'
  - id: B
    component:
      id: {{component_file}}
      with:
        new_variable: B
        value: '2'
",
    data = list(component_file = component_file)
  )

  adam_specifications <- setup_study_dir(list(
    "adlb" = yml
  ))

  expect_no_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = withr::local_tempdir(),
      check_cross_domain_adam_dependencies = FALSE
    )
  )
})


test_that("Within-domain parameter conflicts are reported for every affected domain", {
  component_file <- withr::local_tempfile(fileext = ".mustache")
  writeLines(
    "
#' @title fn_AB
#' @description Multi-output parameterized test component
#' @param param_1 `character` A test parameter
#' @param param_2 `character` A test parameter
#' @param domain `character` The domain name
#' @type derivation
#' @depends {{domain}} USUBJID
#' @outputs A
#' @outputs B
#' @code
{{domain}} <- {{domain}} |>
  dplyr::mutate(A = USUBJID, B = USUBJID)
",
    con = component_file
  )

  yml_adsl <- whisker::whisker.render(
    "
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
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
        domain: ADSL
  - id: B
    component:
      id: {{component_file}}
      with:
        param_1: x
        param_2: y
        domain: ADSL
",
    data = list(component_file = component_file)
  )

  yml_adlb <- whisker::whisker.render(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
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
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
        domain: ADLB
  - id: B
    component:
      id: {{component_file}}
      with:
        param_1: x
        param_2: y
        domain: ADLB
",
    data = list(component_file = component_file)
  )

  adam_specifications <- setup_study_dir(list(
    "adsl" = yml_adsl,
    "adlb" = yml_adlb
  ))

  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = withr::local_tempdir(),
      check_cross_domain_adam_dependencies = FALSE
    ),
    regexp = "different parameter values"
  )
})


test_that("Same component reused across domains with different parameters succeeds", {
  component_file <- withr::local_tempfile(fileext = ".mustache")
  writeLines(
    "
#' @title fn_A
#' @description Single-output parameterized test component
#' @param param_1 `character` A test parameter
#' @param param_2 `character` A test parameter
#' @param domain `character` The domain name
#' @type derivation
#' @depends {{domain}} USUBJID
#' @outputs A
#' @code
{{domain}} <- {{domain}} |>
  dplyr::mutate(A = USUBJID)
",
    con = component_file
  )

  yml_adsl <- whisker::whisker.render(
    "
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
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
        domain: ADSL
",
    data = list(component_file = component_file)
  )

  yml_adlb <- whisker::whisker.render(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
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
      id: {{component_file}}
      with:
        param_1: x
        param_2: y
        domain: ADLB
",
    data = list(component_file = component_file)
  )

  adam_specifications <- setup_study_dir(list(
    "adsl" = yml_adsl,
    "adlb" = yml_adlb
  ))

  expect_no_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = withr::local_tempdir(),
      check_cross_domain_adam_dependencies = FALSE
    )
  )
})


test_that("Same component with identical parameters across invocations passes", {
  component_file <- withr::local_tempfile(fileext = ".mustache")
  writeLines(
    "
#' @title fn_AB
#' @description Multi-output parameterized test component
#' @param param_1 `character` A test parameter
#' @param param_2 `character` A test parameter
#' @type derivation
#' @depends ADLB USUBJID
#' @outputs A
#' @outputs B
#' @code
ADLB <- ADLB |>
  dplyr::mutate(A = USUBJID, B = USUBJID)
",
    con = component_file
  )

  yml <- whisker::whisker.render(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys: [USUBJID]
population:
  base:
    - domain: LB
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
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
  - id: B
    component:
      id: {{component_file}}
      with:
        param_1: '1'
        param_2: '2'
",
    data = list(component_file = component_file)
  )

  adam_specifications <- setup_study_dir(list(
    "adlb" = yml
  ))

  expect_no_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = withr::local_tempdir(),
      check_cross_domain_adam_dependencies = FALSE
    )
  )
})
