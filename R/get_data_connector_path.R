#' Get the path to the data connector for a given trial and data connection type
#'
#' @description This function generates the path to the data connector based on
#' the \code{type} (\code{sdtm}, \code{adam} or \code{metadata}),
#' \code{data_connection} type and the trial metadata.
#'
#' @param type A character string indicating the type of data connector. Valid
#' values are \code{type} (\code{sdtm}, \code{adam} or \code{metadata}.
#' @param trial_metadata A list containing trial metadata. It must contain the
#' \code{project_id}, \code{trial_id}, \code{complete_id}, and \code{instance}
#' @param data_connection A character string indicating the type of data
#' connection. Valid values are \code{connector} (returning a path in line with
#' current NN UNIX-SCE requirement), \code{custom_data} or \code{pharmaverse}
#'
#' @note
#' This will become obsolete when connector config is used instead.
get_data_connector_path <- function(type = c("sdtm", "adam", "metadata"),
                                    trial_metadata,
                                    data_connection = c("connector", "custom_data"),
                                    custom_data_path = NULL) {
  assert_valid_trial_config(trial_metadata)
  data_path <- NULL
  if (data_connection == "connector") {
    connector_path <- "~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/{trial_metadata$instance}"
    data_path <- paste0(connector_path, ifelse(type == "sdtm", "/dm/data/", "/stats/data/"), type)
  } else if (data_connection == "custom_data") {
    data_path <- paste0(custom_data_path, "/", type)
  } else if (data_connection != "pharmaverse") {
    stop("Invalid data connection type. Use 'connector' or 'custom_data'.")
  }
  return(data_path)
}
