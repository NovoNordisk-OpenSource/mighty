#' @title Generate external data code
#'
#' @param payload
#' @param trial_metadata
#' @param sdtm_dataset_list
#' @param path_output
#'
#' @return
#' @export
#'
#' @examples
generate_external_data_code <- function(payload,
                                        trial_metadata,
                                        sdtm_dataset_list,
                                        path_output) {
  # for each element of payload, apply the following logic
  by_domain <- split(payload, payload$domain)

  connector_setup <- glue::glue(
    "cnt <- connector::connect(config = '", path_output, "/_connector.yml') \n")
  data_load_code <-
    purrr::imap(by_domain,
                for_each_domain_connector,
                sdtm_dataset_list)

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
