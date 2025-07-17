assert_valid_outputs <- function(x) {

  # Check that no columns are outputted in multiple column actions per ADaM domain
  is_column_action <- is.na(x$type) | substr(x$type, 1, 3) != "row"
  x_split <- x[is_column_action, c("domain", "depend_cols", "outputs")] |> 
    split(by = "domain")

  # Process each domain to detect errors and collect them
  error_list <- lapply(x_split, function(y) {
    freqs <- table(unlist(y$outputs))
    cols_multiple_output <- names(freqs)[freqs > 1]

    # If multiple columns are outputted, return the error details
    if (length(cols_multiple_output) > 0) {
      return(error_duplicate_columns_outputed(y$domain[1], cols_multiple_output))
    } else {
      return(NULL)
    }
  })

  # Remove NULL entries (domains without errors)
  error_list <-  error_list[!sapply(error_list, is.null)]

  # If errors are found, stop with a summarized error message
  if (length(error_list) > 0) {
    stop(paste(error_list, collapse = "\n"))
  }

  return(invisible(x))
}

error_duplicate_columns_outputed <- function(domain, multiple_output_cols) {
  paste0(
    "Column(s) ", paste(multiple_output_cols, collapse = ", "),
    " are outputted in multiple actions in domain ", domain, "."
  )
}
