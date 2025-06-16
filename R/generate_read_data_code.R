#' @title Generate external data code
#'
#' @param payload
#' @param trial_metadata
#' @param sdtm_dataset_list
#' @param path_output
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_read_data_code <- function(payload,
                                    trial_metadata,
                                    sdtm_dataset_list,
                                    data_connection,
                                    path_output,
                                    .self) {

  block_header <- glue::glue("

# Read data sets ------------------------------------------------
      ")

  # for each element of payload, apply the following logic
  by_domain <- split(payload, payload$domain)
  if (data_connection == "pharmaverse") {
    connector_setup <- NULL
    data_load_code <-
      purrr::imap(by_domain, for_each_domain_pharmaverse, path_output = path_output)
    data_load_code <- Filter(Negate(is.null), data_load_code)
  }
  else {
    connector_setup <- glue::glue("cnt <- connector::connect(config = '",
                                  path_output,
                                  "/_connector.yml') \n")
    data_load_code <-
      purrr::imap(by_domain, for_each_domain_connector, sdtm_dataset_list)

  }

  return(paste0(paste0(c(block_header, connector_setup, data_load_code),
                collapse = "\n\n"), "\n"))
}

external_data <- function(data_type = c("sdtm", "adam", "metadata"),
                          domain,
                          keep_vars,
                          dataset_list = NULL) {
  glue::glue(
    "{domain} <- cnt${data_type}$read_cnt('{domain}') |> ",
    "dplyr::select({keep_vars})"
  )
}

for_each_domain_connector <- function(i, domain_name, sdtm_dataset_list) {
  keep_vars <- i[["column_name"]] |>
    toupper() |>
    unique() |>
    sort() |>
    paste0(collapse = ", ")

  data_load_code <- switch(
    i$domain_type[[1]],
    sdtm = external_data("sdtm", domain_name, keep_vars, sdtm_dataset_list),
    adam = external_data("adam", domain_name, keep_vars),
    md = external_data("metadata", domain_name, keep_vars)
  )
}

for_each_domain_pharmaverse <- function(i, domain_name, path_output) {
  keep_vars <- i[["column_name"]] |>
    toupper() |>
    unique() |>
    sort() |>
    paste0(collapse = ", ")

  data_load_code <- switch(
    i$domain_type[[1]],
    sdtm = pharmaverse_sdtm(domain_name, keep_vars),
    adam = pharmaverse_adam(domain_name, keep_vars, path_output)
  )
}

pharmaverse_sdtm <- function(sdtm_main, keep_vars) {
  return(
    glue::glue(
      "{sdtm_main} <- pharmaversesdtm::{tolower(sdtm_main)} |>
    dplyr::select({keep_vars})"
    )
  )
}

pharmaverse_adam <- function(.self, keep_vars, path_output) {
  path_domain <- file.path(path_output, paste0(.self, ".R"))
  glue::glue(
    "{.self} <- readRDS(\"{path_domain}\") |>
    tibble::as_tibble() |>
    dplyr::select({keep_vars})"
  )
}
