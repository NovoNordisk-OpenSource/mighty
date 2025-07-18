#' @title Generate external data code
#'
#' @param payload
#' @param domain
#' @param path_trial
#'
#' @return
#' @export
#'
#' @examples
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
  )
)

}

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
