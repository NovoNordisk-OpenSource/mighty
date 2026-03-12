#' Generate Parameters for External Data Reading Code
#'
#' Prepares template parameters for generating R code that reads external
#'  data sets and selects required columns based on payload specifications.
#'
#' @param payload Character vector. Variable specifications in "domain.variable"
#'   format (e.g., c("dm.USUBJID", "vs.VSTESTCD", "adsl.AGE")).
#' @param domain Character. Target ADaM domain name for self-reference detection.
#' @param path_connector_config Character string. Path to the directory
#'   containing the connector configuration file (`_connector.yml`).
#'   Prefix with `!expr ` to embed an R expression that is evaluated at
#'   runtime by the generated program (e.g., `'!expr Sys.getenv("TRIAL_PATH")'`).
#'
#' @return Named list with template parameters:
#'   \describe{
#'     \item{connector_path_expr}{Character. A `file.path()` call as R code that
#'       resolves the connector config path. The first argument is either a quoted
#'       literal directory path or an R expression.}
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
#' dm <-  read_domain(file.path(path_connector_config, "sdtm", "dm.xpt")) |>
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
#' the `_connector.yml` path. Supports `!expr ` prefix for runtime expressions
#' and plain directory paths.
#'
#' @param path_connector_config Character string. Directory path or
#'   `!expr `-prefixed R expression.
#'
#' @return Character string. A `file.path()` call as R code that appends
#'   `"_connector.yml"` to the path at runtime.
#' @noRd
make_connector_path_expr <- function(path_connector_config) {
  if (startsWith(path_connector_config, "!expr ")) {
    expr_code <- sub("^!expr ", "", path_connector_config)
    paste0("file.path(", expr_code, ", \"_connector.yml\")")
  } else {
    path_connector_config <- gsub("\\\\", "/", path_connector_config)
    paste0("file.path(\"", path_connector_config, "\", \"_connector.yml\")")
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
    is_self_domain = domain_name == .self,
    domain_name = domain_name,
    data_type = data_type,
    keep_vars = keep_vars
  )
}
