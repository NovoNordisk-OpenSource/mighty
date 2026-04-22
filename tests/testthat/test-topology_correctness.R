test_that("Validation warning occurs when component uses ADSL implicitly without declaring @depends on ADSL", {
  # SETUP -------------------------------------------------------------------

  # Custom component that uses ADSL but only declares dependency on
  # external dataset (dm). Missing dependency: ADSL.USUBJID
  component <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @outputs BRDATE
#' @code
  ADSL <- ADSL %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADSL implicitly
    BRDATE = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    ), by = USUBJID)
  )"
  component_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component), component_file)

  # YAML spec for ADSL with this component
  yaml_content <- paste0(
    "
  id: ADSL
  label: Subject Level Analysis Dataset
  class: SUBJECT LEVEL ANALYSIS DATASET
  structure: One record per subject
  keys:
    - USUBJID
  population:
    base:
      - domain: DM
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: BRDATE
      component:
          id: ",
    component_file
  )

  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))

  # ACT ---------------------------------------------------------------------
  err <- expect_error(
    {
      actual <- generate_adam_code(
        adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = TRUE
      )
    },
    "Specification validation errors found"
  )
  # Check that error message contains the component derivation info
  expect_true(
    any(grepl("Component deriving BRDATE", err$body))
  )
  # Check that suggestions mention @depends annotations
  expect_true(
    any(grepl("@depends annotations", err$body))
  )
})

test_that("Topology is generated correctly when component declares dependencies on both ADSL and external datasets", {
  # SETUP -------------------------------------------------------------------
  trial_path <- withr::local_tempdir()

  # Custom component that uses ADSL and declares dependency on ADSL and  external dataset
  component <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @depends ADSL USUBJID
#' @outputs BRDATE
#' @code
  ADSL <- ADSL %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADSL implicitly
    BRDATE = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    )
), by = USUBJID)"
  component_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component), component_file)

  # YAML spec for ADSL with this component
  yaml_content <- paste0(
    "
  id: ADSL
  label: Subject Level Analysis Dataset
  class: SUBJECT LEVEL ANALYSIS DATASET
  structure: One record per subject
  keys:
    - USUBJID
  population:
    base:
      - domain: DM
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: BRDATE
      component:
          id: ",
    component_file
  )

  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))
  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = get_connector_config_path(trial_path),
    check_cross_domain_adam_dependencies = TRUE
  )

  write_adam_programs(
    dir = trial_path,
    programs = actual$programs
  )
  x <- list.files(trial_path, pattern = ".R", full.names = TRUE)
  programs <- x |> lapply(readLines)
  names(programs) <- basename(x)
  # EXPECT ------------------------------------------------------------------

  # Test ADSL-init_domain comes before ADSL-BRDATE
  expect_section_order(
    "ADSL-init_domain",
    "ADSL-BRDATE",
    programs[["1_ADSL.R"]]
  )
})

test_that("Warning message lists all output columns from component with missing ADLB dependency", {
  # SETUP -------------------------------------------------------------------

  # Custom component that uses ADLB without declaring dependency on ADLB
  component <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @outputs BRDATE
#' @outputs BRDATE2
#' @code
  ADLB <- ADLB %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADLB implicitly
    BRDATE = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    ),
    BRDATE2 = BRDATE), by = USUBJID
  )"
  component_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component), component_file)

  # YAML spec for ADSL (with only col_copy) and ADLB with this component
  yaml_content <- paste0(
    "
  id: ADLB
  label: Laboratory Test Results (Analysis Dataset)
  class: BASIC DATA STRUCTURE
  structure: One record per subject, visit, paramcd
  keys: ['USUBJID', 'AVISITN', 'PARAMCD']
  population:
    base:
      - domain: LB
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: AVISITN
    - id: PARAMCD

    - id: BRDATE
      component:
          id: ",
    component_file,
    "
    - id: BRDATE2
      component:
          id: ",
    component_file
  )

  # The ADSL domain in this test is simple in the sense that it does not create edges in
  # the dependency graph because all ADSL variables are strict col_copy actions. Although no
  # edges are created for ADSL, this does not make the topology for ADSL invalid as this
  # is per design of the \code{make_edges()}
  # In this test case, there are no edges in the dependency graph.
  adam_specifications <- setup_study_dir(list(
    "adsl" = readLines(test_path("fixtures", "skeleton_adsl.yml")),
    "adlb" = yaml_content
  ))
  # ACT ---------------------------------------------------------------------
  err <- expect_error(
    {
      actual <- generate_adam_code(
        adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = TRUE
      )
    },
    "Specification validation errors found"
  )
  # EXPECT ------------------------------------------------------------------
  # Check that error message contains the component derivation info for both columns
  expect_true(
    any(grepl("Component deriving .*BRDATE.*BRDATE2", err$body))
  )
  # Check that suggestions mention @depends annotations
  expect_true(
    any(grepl("@depends annotations", err$body))
  )
})

test_that("Warn if two domains but one has component with missing depends", {
  # SETUP -------------------------------------------------------------------

  # Custom component that uses ADLB without declaring dependency on ADLB
  component <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @outputs BRDATE
#' @outputs BRDATE2
#' @code
  ADLB <- ADLB %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADLB implicitly
    BRDATE = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    ),
    BRDATE2 = BRDATE), by = USUBJID
  )"
  component_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component), component_file)

  # YAML spec for ADSL (with only col_copy) and ADLB with this component
  yaml_content <- paste0(
    "
  id: ADLB
  label: Laboratory Test Results (Analysis Dataset)
  class: BASIC DATA STRUCTURE
  structure: One record per subject, visit, paramcd
  keys: ['USUBJID', 'AVISITN', 'PARAMCD']
  population:
    base:
      - domain: LB
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: AVISITN
    - id: PARAMCD

    - id: BRDATE
      component:
          id: ",
    component_file,
    "
    - id: BRDATE2
      component:
          id: ",
    component_file
  )

  # The ADSL domain in this test is more complex and it has edges in
  # the dependency graph. It is important to check that these edges does not
  # influence the validation of the edges in the ADLB domain

  # Process ADSL fixture which contains {path_base} glue placeholders
  adsl_content <- readLines(test_path(
    "fixtures",
    "supplementary_data_adsl_01.yml"
  ))
  path_base <- test_path()
  adsl_processed <- as.character(glue::glue(
    paste(adsl_content, collapse = "\n"),
    path_base = path_base
  ))

  # ADLB content has no glue placeholders, use directly
  adam_specifications <- setup_study_dir(list(
    "adsl" = adsl_processed,
    "adlb" = yaml_content
  ))

  # ACT ---------------------------------------------------------------------
  err <- expect_error(
    {
      actual <- generate_adam_code(
        adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = TRUE
      )
    },
    "Specification validation errors found"
  )
  # EXPECT ------------------------------------------------------------------
  # Check that error message contains the component derivation info for both columns
  expect_true(
    any(grepl("Component deriving .*BRDATE.*BRDATE2", err$body))
  )
  # Check that suggestions mention @depends annotations
  expect_true(
    any(grepl("@depends annotations", err$body))
  )
})

test_that("Validation with mix of components warning when no @depends on component", {
  # SETUP -------------------------------------------------------------------

  # Custom component that uses ADSL but only declares dependency on
  # external dataset (dm). Missing dependency: ADSL.USUBJID
  component_invalid <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @outputs BRDATE
#' @code
  ADSL <- ADSL %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADSL implicitly
    BRDATE = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    )), by = USUBJID
  )"
  component_invalid_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component_invalid), component_invalid_file)

  # Custom component that uses ADSL and declares dependency on actual and external dataset
  component_valid <- "
#' @title BRDATEOK valid
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @depends ADSL USUBJID
#' @outputs BRDATEOK
#' @code
  ADSL <- ADSL %>% dplyr::left_join(dm |> dplyr::mutate(  # Uses ADSL implicitly
    BRDATEOK = dplyr::case_when(
      nchar(BRTHDTC) >= 10 ~ BRTHDTC
    )), by = USUBJID
  )"
  component_valid_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component_valid), component_valid_file)

  # YAML spec for ADSL with this component
  yaml_content <- paste0(
    "
  id: ADSL
  label: Subject Level Analysis Dataset
  class: SUBJECT LEVEL ANALYSIS DATASET
  structure: One record per subject
  keys:
    - USUBJID
  population:
    base:
      - domain: DM
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: BRDATE
      component:
          id: ",
    component_invalid_file,
    "
    - id: BRDATEOK
      component:
          id: ",
    component_valid_file
  )
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))
  # ACT ---------------------------------------------------------------------
  err <- expect_error(
    {
      actual <- generate_adam_code(
        adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = TRUE
      )
    },
    "Specification validation errors found"
  )
  # Check that error message contains the component derivation info
  expect_true(
    any(grepl("Component deriving BRDATE", err$body))
  )
  # Check that suggestions mention @depends annotations
  expect_true(
    any(grepl("@depends annotations", err$body))
  )
  # Check that BRDATEOK (which has proper @depends) is NOT mentioned
  expect_false(
    any(grepl("Component deriving BRDATEOK", err$body))
  )
})

test_that("Validation with two domains throws warning when no @depends on component", {
  # SETUP -------------------------------------------------------------------

  # Custom component that uses ADSL but only declares dependency on
  # external dataset (dm). Missing dependency: ADSL.USUBJID
  component_invalid <- "
#' @title BRDATE
#' @description Treatment
#' @type derivation
#' @depends dm BRTHDTC
#' @outputs BRDATE
#' @code
'not important'"
  component_invalid_file <- withr::local_tempfile(
    fileext = ".R"
  )
  writeLines(as.character(component_invalid), component_invalid_file)

  # YAML spec for ADSL with this component
  yaml_content_ADSL <- paste0(
    "
  id: ADSL
  label: Subject Level Analysis Dataset
  class: SUBJECT LEVEL ANALYSIS DATASET
  structure: One record per subject
  keys:
    - USUBJID
  population:
    base:
      - domain: DM
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: BRDATE
      component:
          id: ",
    component_invalid_file
  )

  # YAML spec for ADLB with this component
  yaml_content_ADLB <- paste0(
    "
  id: ADLB
  label: Laboratory Test Results (Analysis Dataset)
  class: BASIC DATA STRUCTURE
  structure: One record per subject, visit, paramcd
  keys: ['USUBJID', 'AVISITN', 'PARAMCD']
  population:
    base:
      - domain: LB
        depends: NA
        filter: NA
  columns:
    - id: USUBJID
    - id: AVISITN
    - id: PARAMCD

    - id: BRDATE
      component:
          id: ",
    component_invalid_file
  )
  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content_ADSL,
    "adlb" = yaml_content_ADLB
  ))
  # ACT ---------------------------------------------------------------------
  err <- expect_error(
    {
      actual <- generate_adam_code(
        adam_specifications = adam_specifications,
        path_connector_config = get_temp_connector_config_path(),
        check_cross_domain_adam_dependencies = TRUE
      )
    },
    "Specification validation errors found"
  )
  # Check that both nodes are mentioned in the error
  expect_true(any(grepl("Node ADLB-BRDATE", err$body)))
  expect_true(any(grepl("Node ADSL-BRDATE", err$body)))
  # Check that error message contains the component derivation info
  expect_true(
    any(grepl("Component deriving BRDATE", err$body))
  )
  # Check that suggestions mention @depends annotations
  expect_true(
    any(grepl("@depends annotations", err$body))
  )
})

test_that("Error when ADaM specification is missing init_domain (no population.base)", {
  # This test shows the only way a user could encounter a validate_init_domain_presence()
  #  error. This pathways is very unlikely for a user, but we are keeping the
  #  validate_init_domain_presence() check as a defensive programming against introducing
  #  errors later when refactoring.
  # SETUP -------------------------------------------------------------------

  # Component that has no dependencies - creates orphaned node
  component_brdate <- "
#' @title BRDATE
#' @description Birth date derivation with no dependencies
#' @type derivation
#' @outputs BRDATE
#' @code
  ADSL <- ADSL %>% dplyr::mutate(BRDATE = NA_character_)
  "
  component_file <- withr::local_tempfile(fileext = ".R")
  writeLines(as.character(component_brdate), component_file)

  # YAML spec for ADSL with component but NO col_copy columns
  # This means no init_domain node will be created even though population.base exists
  yaml_content <- paste0(
    "
  id: ADSL
  label: Subject Level Analysis Dataset
  class: SUBJECT LEVEL ANALYSIS DATASET
  structure: One record per subject
  keys:
    - BRDATE
  population:
    base:
      - domain: DM
        depends: NA
        filter: NA
  columns:
    - id: BRDATE
      component:
          id: ",
    component_file
  )

  adam_specifications <- setup_study_dir(list(
    "adsl" = yaml_content
  ))

  # ACT & EXPECT ------------------------------------------------------------

  expect_error(
    generate_adam_code(
      adam_specifications = adam_specifications,
      path_connector_config = get_temp_connector_config_path(),
      check_cross_domain_adam_dependencies = TRUE
    ),
    "The init_domain node is missing for ADSL"
  )
})
