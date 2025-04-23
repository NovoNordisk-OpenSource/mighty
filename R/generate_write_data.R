generate_write_data <- function(domain_name, data_connection, path_output) {
  if (data_connection == "pharmaverse") {
    file_path <- file.path(path_output, paste0(domain_name, ".R"))
    glue::glue("saveRDS(object = {domain_name}, file = \"{file_path}\")")
  }

  }
