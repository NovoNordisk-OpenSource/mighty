#' Generate Parameters for External Data Reading Code
#'
#' Prepares template parameters for generating R code that reads external
#'  data sets and selects required columns based on payload specifications.
#'
#' @param payload Character vector. Variable specifications in "domain.variable"
#'   format (e.g., c("DM.USUBJID", "VS.VSTESTCD", "ADSL.AGE")).
#' @param domain Character. Target ADaM domain name for self-reference detection.
#' @param path_connector_config Character string. File path to the connector
#'   configuration file (e.g., `_connector.yml`). This path is embedded
#'   verbatim into the generated programs.
#'   Prefix with `!expr ` to embed an R expression that is evaluated at
#'   runtime by the generated program (e.g., `'!expr here::here("_connector.yml")'`).
#'
#' @return Named list with template parameters:
#'   \describe{
#'     \item{connector_path_expr}{Character. An R code expression that evaluates
#'       to the connector config file path. Either a quoted literal path or an
#'       R expression.}
#'     \item{domains}{List. Domain specifications with elements:
#'       \code{is_current_domain}, \code{domain_name}, \code{data_type}, \code{keep_vars}}
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
#' DM <- read_domain(file.path(path_connector_config, "sdtm", "dm.xpt")) |>
#'   select(USUBJID, AGE, SEX)
#' }
#' @noRd
params_read_data_code <- function(payload, domain, path_connector_config) {
  # Whisker template

  v <- strsplit(payload, "\\.")
  payload_dt <- data.table(
    column_name = sapply(v, `[`, 2),
    domain = sapply(v, `[`, 1)
  )
  payload_dt[["domain_type"]] <- classify_data_domains(payload_dt$domain)

  # Prepare template data
  by_domain <- split(payload_dt, by = "domain")

  connector_path_expr <- make_connector_path_expr(path_connector_config)

  return(list(
    connector_path_expr = connector_path_expr,
    domains = purrr::imap(by_domain, prepare_domain_data, .self = domain) |>
      unname()
  ))
}

#' Build connector path expression
#'
#' Converts a `path_connector_config` value into an R code string that resolves
#' the connector config file path. Supports `!expr ` prefix for runtime
#' expressions and plain file paths.
#'
#' @param path_connector_config Character string. File path or
#'   `!expr `-prefixed R expression.
#'
#' @return Character string. An R expression as code that evaluates to the
#'   connector config file path.
#' @noRd
make_connector_path_expr <- function(path_connector_config) {
  if (!nzchar(path_connector_config)) {
    cli::cli_abort(
      "{.arg path_connector_config} must be a non-empty file path."
    )
  }

  if (startsWith(path_connector_config, "!expr ")) {
    sub("^!expr ", "", path_connector_config)
  } else {
    paste0('"', gsub("\\\\", "/", path_connector_config), '"')
  }
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
    is_current_domain = domain_name == .self,
    domain_name = domain_name,
    data_type = data_type,
    keep_vars = keep_vars
  )
}
