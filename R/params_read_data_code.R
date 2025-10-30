#' Generate Parameters for External Data Reading Code
#'
#' Prepares template parameters for generating R code that reads external
#'  data sets and selects required columns based on payload specifications.
#'
#' @param payload Character vector. Variable specifications in "domain.variable"
#'   format (e.g., c("dm.USUBJID", "vs.VSTESTCD", "adsl.AGE")).
#' @param domain Character. Target ADaM domain name for self-reference detection.
#' @param path_trial Character. File path to trial data directory.
#'
#' @return Named list with template parameters:
#'   \describe{
#'     \item{path_trial}{Character. Trial data path}
#'     \item{domains}{List. Domain specifications with elements:
#'       \code{is_self_domain}, \code{domain_name}, \code{data_type}, \code{keep_vars}}
#'   }
#'
#' @details
#' Processes payload by:
#' \itemize{
#'   \item Parsing "domain.variable" specifications
#'   \item Classifying domains using [classify_data_domains()]
#'   \item Grouping variables by domain with uppercase formatting
#'   \item Detecting self-domain references for template optimization
#' }
#'
#' Generated code typically follows this pattern:
#' \preformatted{
#' dm <-  read_domain(file.path(path_trial, "sdtm", "dm.xpt")) |>
#'   select(USUBJID, AGE, SEX)
#' }
#'
#' @examples
#' \dontrun{
#' params <- params_read_data_code(
#'   payload = c("dm.USUBJID", "dm.AGE", "vs.VSTESTCD"),
#'   domain = "adae",
#'   path_trial = "/path/to/data"
#' )
#'
#' # Check domain specifications
#' params$domains[[1]]$keep_vars  # "AGE, USUBJID"
#' params$domains[[1]]$data_type  # "sdtm"
#' }
#' @noRd
params_read_data_code <- function(payload, domain, path_trial) {
  # Whisker template

  v <- strsplit(payload, "\\.")
  payload_dt <- data.table(
    column_name = sapply(v, `[`, 2),
    domain = sapply(v, `[`, 1)
  )
  payload_dt[["domain_type"]] <- classify_data_domains(payload_dt$domain)

  # Prepare template data
  by_domain <- split(payload_dt, by = "domain")
  return(list(
    path_trial = path_trial,
    domains = purrr::imap(by_domain, prepare_domain_data, .self = domain) |>
      unname()
  ))
}

#' Prepare Domain-Specific Data Reading Parameters
#'
#' Helper function that formats domain variables for data reading templates.
#' Called internally by [params_read_data_code()] for each domain.
#'
#' @param domain_data data.table with column_name and domain_type columns.
#' @param domain_name Character. Source domain name.
#' @param .self Character. Target domain for self-reference detection.
#'
#' @return Named list with domain reading specifications.
#' @noRd
prepare_domain_data <- function(domain_data, domain_name, .self) {
  keep_vars <- domain_data[["column_name"]] |>
    toupper() |>
    unique() |>
    sort() |>
    paste0(collapse = ", ")

  data_type <- switch(
    domain_data$domain_type[[1]],
    sdtm = "sdtm",
    adam = "adam",
    md = "metadata"
  )

  list(
    is_self_domain = domain_name == .self,
    domain_name = domain_name,
    data_type = data_type,
    keep_vars = keep_vars
  )
}
