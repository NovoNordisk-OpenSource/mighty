test_that("Only YAML-defined columns appear in the final dataset", {
  # SETUP -------------------------------------------------------------------

  component <- "
#' @title Convert DTC to DT
#' @description Converts DTC to date (DT) format and computes the corresponding date flag (DTF) for the ADLB dataset.
#' @type derivation
#' @depends LB USUBJID
#' @depends LB STUDYID
#' @depends LB LBSEQ
#' @depends LB LBDTC
#' @depends ADLB USUBJID
#' @depends ADLB STUDYID
#' @depends ADLB LBSEQ
#' @outputs ASTDT
#' @outputs ASTDTF
#' @code
ADLB <- ADLB |>
  dplyr::left_join(
    LB |>
      dplyr::mutate(
        ASTDT = admiral::convert_dtc_to_dt(
          dtc = LBDTC,
          highest_imputation = \"M\",
          date_imputation = \"first\"
        ),
        ASTDTF = admiral::compute_dtf(
          dtc = LBDTC,
          dt = ASTDT
        )
      ),
    by = c(\"USUBJID\", \"STUDYID\", \"LBSEQ\")
  )"
  component_file <- withr::local_tempfile(fileext = ".R")
  writeLines(as.character(component), component_file)

  yaml_content_adlb <- paste0(
    "
id: ADLB
label: Laboratory Analysis Dataset
class: BASIC DATA STRUCTURE
structure: One record per subject per parameter per analysis visit
keys:
  - USUBJID
  - STUDYID
  - LBSEQ
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

  - id: STUDYID

  - id: LBSEQ

  - id: ASTDT
    component:
      id: ",
    component_file,
    "

  - id: ASTDTF
    component:
      id: ",
    component_file
  )

  mighty_yml_content <- "keys: {}"
  adam_specifications <- setup_study_dir(list(
    "adlb" = yaml_content_adlb,
    "_mighty" = mighty_yml_content
  ))

  path_connector_config <- withr::local_tempdir()

  setup_testdata(
    testdata = "pharmaverse",
    test_data_path = path_connector_config,
    sdtm_domains = c("lb")
  )

  # ACT ---------------------------------------------------------------------

  actual <- generate_adam_code(
    adam_specifications = adam_specifications,
    path_connector_config = path_connector_config,
    check_cross_domain_adam_dependencies = FALSE
  )

  write_adam_programs(
    dir = path_connector_config,
    programs = actual$programs,
    style = TRUE
  )
  x <- list.files(path_connector_config, pattern = "\\.R$", full.names = TRUE)

  # ASSERT ------------------------------------------------------------------
  expect_no_error(source(x[[1]]))
  expect_gt(nrow(ADLB), 0)

  # Final dataset contains exactly the columns defined in the YAML spec.
  # Source columns like LBDTC (@depends LB LBDTC) must not leak through.
  expect_setequal(
    names(ADLB),
    c("USUBJID", "STUDYID", "LBSEQ", "ASTDT", "ASTDTF")
  )
})
