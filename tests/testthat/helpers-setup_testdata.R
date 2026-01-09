#' Prepare Test Data for Unit Tests
#'
#' The `setup_testdata` function prepares test data required for unit testing.
#' Currently it extracts \code{pharmaversesdtm} and \code{pharmaverseadam}
#' datasets and sets up the test environment by storing
#' the relevant domains as data set files (\code{.parquet}) in a temporary
#' directory.
#' Under \code{test_data_path}, a folder \code{data} is created, and in here
#' the folders
#' \code{sdtm}, \code{adam}, and \code{metadata} are created.
#' The \code{connector} package is used \todo(can we link to where the data
#' is loaded, e.g.
#' \code{generate_external_data_code} and \code{generate_write_domain}?)
#' when creating ADaM
#' programs. Since the path to test data is expected to be dynamic, a
#' symbolic link is used
#' to link the test data area. The symbolic link \code{data} is created in
#' the
#' \code{tests\testthat\fixtures} folder and is removed when the parent
#' (test-)function exits.
#'
#' @param testdata Character. Currently only \code{pharmaverse} is supported.
#' @param test_data_path Character. The directory path where test data
#'   should be created or loaded.
#' @param sdtm_domains Character vector. A list of SDTM domains to store as
#'   \code{.parquet} files.
#' @param adam_domains Character vector. A list of ADaM domains to store as
#'   \code{.parquet} files.
#' @return Invisibly returns the path to the test data directory.
#' @details
#' This helper is intended to be used in the setup phase of testthat test files.
#'
#' @examples
#' setup_testdata(test_data_path = "tests/testthat/testdata",
#'                sdtm_domains = c("dm", "suppdm", "ds", "suppds")
#'
#' @seealso [testthat::setup()], [withr::local_tempdir()]
#' @export
setup_testdata <- function(
  testdata = c("pharmaverse"),
  test_data_path,
  sdtm_domains = c("dm", "suppdm", "dm_vaccine", "ae", "lb", "sv"),
  adam_domains = c(),
  remove_cols = NULL
) {
  testdata <- match.arg(testdata)

  if (testdata == "pharmaverse") {
    # copy and modify connector config
    if (.Platform$OS.type == "windows") {
      # Normalize and convert to forward slashes
      test_data_path <- normalizePath(
        test_data_path,
        winslash = "/",
        mustWork = FALSE
      )
    }
    yaml <- test_path("fixtures", "_connector.yml") |>
      readLines() |>
      paste0(collapse = "\n") |>
      glue::glue(
        root = test_data_path,
        .open = "{{",
        .close = "}}"
      )

    writeLines(yaml, con = file.path(test_data_path, "_connector.yml"))

    # setup temporary data area
    data_path <- file.path(test_data_path, "data")
    sdtm_testdata_path <- file.path(data_path, "sdtm")
    adam_testdata_path <- file.path(data_path, "adam")
    metadata_testdata_path <- file.path(data_path, "metadata")

    dir.create(data_path)
    dir.create(sdtm_testdata_path)
    dir.create(adam_testdata_path)
    dir.create(metadata_testdata_path)

    # create SDTM test data based on pharmaversesdtm
    # loop over sdtm_domains to store data into sdtm_testdata_path
    lapply(
      sdtm_domains,
      function(x) {
        tryCatch(
          {
            dataset <- eval(parse(text = paste0("pharmaversesdtm::", x)))
            if (!is.null(remove_cols)) {
              dataset <- dataset |>
                dplyr::select(-all_of(remove_cols[domain == x]$columns))
            }
            arrow::write_parquet(
              dataset,
              file.path(sdtm_testdata_path, paste0(x, ".parquet"))
            )
          },
          error = function(e) {
            message(
              "Error writing dataset: ",
              e$message,
              "\nPlease ensure the dataset ",
              paste0(x),
              " exists in pharmaversesdtm."
            )
          }
        )
      }
    )
    # create ADaM test data based on pharmaverseadam
    # loop over adam_domains to store data into adam_testdata_path
    lapply(
      adam_domains,
      function(x) {
        tryCatch(
          {
            # pharmaverseadam uses lowercase names, but preserve original case for filename
            dataset <- eval(parse(text = paste0("pharmaverseadam::", x)))
            if (!is.null(remove_cols)) {
              dataset <- dataset |>
                dplyr::select(-all_of(remove_cols[domain == x]$columns))
            }
            arrow::write_parquet(
              dataset,
              file.path(adam_testdata_path, paste0(x, ".parquet"))
            )
          },
          error = function(e) {
            message(
              "Error writing dataset: ",
              e$message,
              "\nPlease ensure the dataset ",
              paste0(x),
              " exists in pharmaverseadam."
            )
          }
        )
      }
    )

    return(test_data_path)
  } else {
    stop("Unsupported test data type.")
  }
}
