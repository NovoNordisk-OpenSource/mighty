#' @title Generate external data code
#'
#' @param payload
#' @param trial_metadata
#' @param path_output
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_read_data_code <- function(payload,
                                    path_output) {
  block_header <- glue::glue("
# Read data sets ------------------------------------------------
      ")

  # for each element of payload, apply the following logic
  by_domain <- split(payload, payload$domain)

  connector_setup <- glue::glue("cnt <- connector::connect(config = '",
                                path_output,
                                "/_connector.yml') \n")
  data_load_code <-
    purrr::imap(by_domain, for_each_domain_connector)


  return(paste0(paste0(
    c(block_header, connector_setup, data_load_code), collapse = "\n\n"
  ), "\n"))
}

external_data <- function(data_type = c("sdtm", "adam", "metadata"),
                          domain,
                          keep_vars
                          ) {
  glue::glue(
    "{domain} <- cnt${data_type}$read_cnt('{domain}') |> ",
    "dplyr::select({keep_vars})"
  )
}

for_each_domain_connector <- function(i, domain_name) {
  keep_vars <- i[["column_name"]] |>
    toupper() |>
    unique() |>
    sort() |>
    paste0(collapse = ", ")

  data_load_code <- switch(
    i$domain_type[[1]],
    sdtm = external_data("sdtm", domain_name, keep_vars),
    adam = external_data("adam", domain_name, keep_vars),
    md = external_data("metadata", domain_name, keep_vars)
  )
}
