#' @title Generate external data code
#'
#' @param payload
#' @param trial_metadata
#' @param sdtm_dataset_list
#' @param adam_dataset_list
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_external_data_code <- function(payload,
                                        trial_metadata,
                                        sdtm_dataset_list,
                                        adam_dataset_list,
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
  if (data_connection == "connector") {
    # Connector setup
    connector_setup <- glue::glue(
      "adam_connector <- connector::connector_fs(path='~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/current/stats/data/adam')
  sdtm_connector <- connector::connector_fs(path='~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/current/dm/data/sdtm')
  md_connector <- connector::connector_fs(path='~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/current/stats/data/metadata')
  "
    )
    data_load_code <-
      purrr::imap(by_domain,
                  for_each_domain_connector,
                  sdtm_dataset_list,
                  adam_dataset_list)

  }

  block_header <- glue::glue(
    "

# LOAD all external datasets needed ------------------------------------------------
      "
  )
  c(block_header, connector_setup, data_load_code)
}

external_data_stdm <- function(sdtm_main,
                               keep_vars,
                               sdtm_dataset_list,
                               file_extension = c("parquet", "sas7bdat")) {
  sdtm_main_ext <- paste(sdtm_main, file_extension[1], sep = ".")
  parquet_exists <- sdtm_main_ext %in% sdtm_dataset_list
  if (!parquet_exists) {
    sdtm_main_ext <- paste(sdtm_main, file_extension[2], sep = ".")
  }

  supp_dataset_name <- paste0('supp', sdtm_main_ext)
  supp_exists <- supp_dataset_name %in% sdtm_dataset_list

  if (supp_exists) {
    return(
      glue::glue(
        "{sdtm_main}_supp <- sdtm_connector |> connector::read_cnt('{supp_dataset_name}')
  {sdtm_main} <- sdtm_connector |>
    connector::read_cnt('{sdtm_main_ext}') |>
    mighty::sdtm_add_supp({sdtm_main}_supp) |>
    dplyr::select({keep_vars})
    rm({sdtm_main}_supp)"
      )
    )
  }

  return(
    glue::glue(
      "{sdtm_main} <- sdtm_connector |>
    connector::read_cnt('{sdtm_main_ext}') |>
    dplyr::select({keep_vars})"
    )
  )
}

external_data_adam <- function(adam_domain,
                               keep_vars,
                               adam_dataset_list,
                               file_extension = c("parquet", "sas7bdat")) {
  adam_domain_ext <- make_adam_domain_ext(adam_domain, file_extension, adam_dataset_list)
  glue::glue(
    "{adam_domain} <- adam_connector |>
    connector::read_cnt('{adam_domain_ext}') |>
    dplyr::select({keep_vars})"
  )
}

external_data_md <- function(md_domain, keep_vars, file_extension = "sas7bdat") {
  md_domain_ext <- paste(md_domain, file_extension, sep = ".")
  glue::glue(
    "{md_domain} <- md_connector |>
    connector::read_cnt('{md_domain_ext}') |>
    dplyr::select({keep_vars})"
  )
}

for_each_domain_connector <- function(i,
                                      domain_name,
                                      sdtm_dataset_list,
                                      adam_dataset_list) {
  keep_vars <- i[["column_name"]] |>
    toupper() |>
    unique() |>
    sort() |>
    paste0(collapse = ", ")


  data_load_code <- switch(
    i$domain_type[[1]],
    sdtm = external_data_stdm(domain_name, keep_vars, sdtm_dataset_list),
    adam = external_data_adam(domain_name, keep_vars, adam_dataset_list),
    md = external_data_md(domain_name, keep_vars)
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
  sdtm_dataset_list <- data(package = "pharmaversesdtm")$results[, "Item"]
  supp_dataset_name <- paste0('supp', sdtm_main)
  supp_exists <- supp_dataset_name %in% sdtm_dataset_list

  if (supp_exists) {
    return(
      glue::glue(
        "{sdtm_main}_supp <- pharmaversesdtm::{tolower(supp_dataset_name)}
  {sdtm_main} <- pharmaversesdtm::{tolower(sdtm_main)} |>
    dplyr::select({keep_vars})
    rm({sdtm_main}_supp)"
      )
    )
  }

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
