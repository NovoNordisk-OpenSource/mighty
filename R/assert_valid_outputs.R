assert_valid_outputs <- function(x) {
browser()
  # Check that no columns are outputted in multiple column actions per ADaM domain
  is_column_action <- is.na(x$type) | substr(x$type, 1, 3) != "row"
  x_split <- split(x[is_column_action, c("domain", "depend_cols", "outputs")],
                   by = "domain")
  output_freqs <- lapply(x_split, function(y) {
    freqs <- table(unlist(y$outputs))
    cols_multiple_output <- names(freqs)[freqs > 1]
    if (length(cols_multiple_output) > 0) {
      stop(paste0("Column(s) ", paste(cols_multiple_output, collapse = ", "),
                  " are outputted in multiple actions in domain ",
                  y$domain[1], "."))
    }
  })
}
