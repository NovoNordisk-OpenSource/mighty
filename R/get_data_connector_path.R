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
