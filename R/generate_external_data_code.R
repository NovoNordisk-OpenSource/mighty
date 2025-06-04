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
generate_external_data_code <- function(payload,
                                        trial_metadata,
                                        sdtm_dataset_list,
                                        data_connection,
                                        path_output) {
  # for each element of payload, apply the following logic
  by_domain <- split(payload, payload$domain)
  if (data_connection == "pharmaverse") {

    connector_setup <- NULL
    data_load_code <-
      purrr::imap(by_domain, for_each_domain_pharmaverse, path_output = path_output)
    data_load_code <- Filter(Negate(is.null), data_load_code)
  }
  else {
    connector_setup <- glue::glue(
      "cnt <- connector::connect(config = '", path_output, "/_connector.yml') \n")
    data_load_code <-
      purrr::imap(by_domain,
                  for_each_domain_connector,
                  sdtm_dataset_list)

  }

  block_header <- glue::glue(
    "

# LOAD all external datasets needed ------------------------------------------------
      "
  )
  c(block_header, connector_setup, data_load_code)
}

external_data <- function(data_type = c("sdtm", "adam", "metadata"),
                          domain,
                          keep_vars,
                          dataset_list = NULL) {
  supp_exists <- FALSE
  if (data_type == "sdtm" && !is.null(dataset_list)) {
    supp_dataset_name <- paste0('supp', domain)
    supp_exists <- supp_dataset_name %in% dataset_list
  }

  glue::glue(
    ifelse(supp_exists, "{domain}_supp <- cnt${data_type}$read_cnt('{supp_dataset_name}')", ""),
    "{domain} <- cnt${data_type}$read_cnt('{domain}') |> ",
    ifelse(supp_exists, "mighty::sdtm_add_supp({sdtm_main}_supp) |>", ""),
    "dplyr::select({keep_vars})",
    ifelse(supp_exists, "rm({domain}_supp)", "")
  )
}

for_each_domain_connector <- function(i,
                                      domain_name,
                                      sdtm_dataset_list) {
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

  data_load_code <- switch(i$domain_type[[1]],
                           sdtm = pharmaverse_sdtm(domain_name, keep_vars),
                           adam = pharmaverse_adam(domain_name, keep_vars, path_output))
}

pharmaverse_sdtm <- function(sdtm_main, keep_vars) {
  # sdtm_dataset_list <- data(package = "pharmaversesdtm")$results[, "Item"]
  # supp_dataset_name <- paste0('supp', sdtm_main)
  # supp_exists <- supp_dataset_name %in% sdtm_dataset_list

  # if (supp_exists) {
  #   return(
  #     glue::glue(
  #       "{sdtm_main}_supp <- pharmaversesdtm::{tolower(supp_dataset_name)}
  # {sdtm_main} <- pharmaversesdtm::{tolower(sdtm_main)} |>
  #   dplyr::select({keep_vars})
  #   rm({sdtm_main}_supp)"
  #     )
  #   )
  # }

  return(
    glue::glue(
      "{sdtm_main} <- pharmaversesdtm::{tolower(sdtm_main)} |>
    dplyr::select({keep_vars})"
    )
  )
}

pharmaverse_adam <- function(adam_domain, keep_vars, path_output) {

  path_domain <- file.path(path_output, paste0(adam_domain, ".R"))
  glue::glue(
    "{adam_domain} <- readRDS(\"{path_domain}\") |>
    tibble::as_tibble() |>
    dplyr::select({keep_vars})"
  )
}
