#' Prepare Test Data for Unit Tests
#'
#' The `setup_testdata` function prepares test data required for unit testing. 
#' Currently it extracts \code{pharmaversesdtm} datasets and sets up the test environment by storing
#' the relevant domains as data set files (\code{.parquet}) in a temporary directory.
#' Under \code{test_data_path}, a folder \code{data} is created, and in here the folders
#' \code{sdtm}, \code{adam}, and \code{metadata} are created.
#' The \code{connector} package is used \todo(can we link to where the data is loaded, e.g. 
#' \code{generate_external_data_code} and \code{generate_write_data}?) when creating ADaM
#' programs. Since the path to test data is expected to be dynamic, a symbolic link is used 
#' to link the test data area. The symbolic link \code{data} is created in the 
#' \code{tests\testthat\fixtures} folder and is removed when the parent (test-)function exits. 
#'
#' @param testdata Character. Currently only \code{pharmaverse} is supported.
#' @param test_data_path Character. The directory path where test data should be created or loaded.
#' @param sdtm_domains Character vector. A list of SDTM domains to store as \code{.parquet} files.
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
setup_testdata <- function(testdata = c("pharmaverse"), test_data_path, sdtm_domains = c("dm", "suppdm", "dm_vaccine", "ae", "lb", "sv")) {

  testdata <- match.arg(testdata)
  # TODO: Should we only allow a set of SDTM domains to be available (in case sdtmpharmaverse gets extended?)
  # Currently: allow any
  # sdtm_domains <- match.arg(sdtm_domains, several.ok = TRUE)

  if (testdata == "pharmaverse") {
    # copy connector config
    file.copy(
      from = file.path(test_path("fixtures", "_connector.yml")),
      to = test_data_path,
      overwrite = TRUE
    )

    # setup temporary data area
    data_path <- file.path(test_data_path, "data")
    sdtm_testdata_path <- file.path(data_path, "sdtm")
    adam_testdata_path <- file.path(data_path, "adam")
    metadata_testdata_path <- file.path(data_path, "metadata")

    dir.create(data_path)
    dir.create(sdtm_testdata_path)
    dir.create(adam_testdata_path)
    dir.create(metadata_testdata_path)

    # create SDTM test data based on pharmaverssdtm
    # loop over sdtm_domains to store data into sdtm_testdata_path
    lapply(
      sdtm_domains,
      function(x) {
        tryCatch({
          dataset <- eval(parse(text = paste0("pharmaversesdtm::", x)))
          arrow::write_parquet(
            dataset,
            file.path(sdtm_testdata_path, paste0(x, ".parquet"))
          )
        }, error = function(e) {
          message("Error writing dataset: ", e$message,
                  "\nPlease ensure the dataset ",
                  paste0(x),
                  " exists in pharmaversesdtm.")
        })
      }
    )

    # Create symbolic link to temporary data and ensure it is removed after test
    # is completed. This is needed to ensure that
    # since the connector needs to link to a static file location, whereas
    # the test data is created in a temporary location that will be cleaned up
    # after testing
    file.symlink(
      data_path,
      test_path("fixtures", "data")
    )
    withr::defer_parent(
      unlink(test_path("fixtures", "data"), recursive = TRUE, force = TRUE)
    )
    return(test_data_path)
  } else {
    stop("Unsupported test data type.")
  }
}
