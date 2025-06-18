generate_write_domain <- function(domain_name,
                                path_output,
                                input_tables) {
  domain_name_upper <- toupper(domain_name)
  block_header <- glue::glue("

# Save {domain_name_upper} ------------------------------------------------
      ")

  save_table_code <- ""
  # Assumption: cnt (Connector) object setup earlier i.e. in
  # generate_read_data_code
  # TODO: the parquet file format is chosen by default. Should there be a
  # setting e.g. in the trial_metadata config file to choose file format?
  save_table_code <- glue::glue(
    '
    cnt$adam$write_cnt({domain_name}, "{domain_name}.parquet", overwrite = TRUE)
    '
  )
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
