generate_write_data <- function(domain_name,
                                data_connection,
                                path_output,
                                input_tables) {
  domain_name_upper <- toupper(domain_name)
  block_header <- glue::glue("

# Save {domain_name_upper} ------------------------------------------------
      ")

  save_table_code <- ""
  if (data_connection == "pharmaverse") {
    file_path <- file.path(path_output, paste0(domain_name, ".R"))
    save_table_code <- glue::glue("saveRDS(object = {domain_name}, file = \"{file_path}\")")
  }
  # Assumption: cnt (Connector) object setup earlier i.e. in
  # generate_external_data_code
  # TODO: the parquet file format is chosen by default. Should there be a
  # setting e.g. in the trial_metadata config file to choose file format?
  else {
    save_table_code <- glue::glue(
      '
      cnt$adam$write_cnt({domain_name}, "{domain_name}.parquet", overwrite = TRUE)
      '
    )
  }
  cleanup_code <- ""
  if(length(input_tables) > 0){
    input_tables <- setdiff(input_tables, "core") # Temporary solution. Core should not be visible
    tables_to_remove <- paste0('c(', paste(shQuote(input_tables), collapse = ", "), ')')
    cleanup_code <- glue::glue(
      '
      # Remove input tables
      rm(list = {tables_to_remove})
      '
    )
  }

  return(c(block_header, save_table_code, cleanup_code))
}
