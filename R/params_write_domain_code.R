params_write_domain_code <- function(.self,
                                     is_final_pgm,
                                     domain_keys,
                                     domain_ui_data) {
  keep_vars <- names(domain_ui_data$columns)[nchar(names(domain_ui_data$columns)) > 0]
  row_order_vars <- domain_keys[[toupper(.self)]]

  if (length(row_order_vars) > 3) {
    row_order_vars <- paste(row_order_vars, collapse = ",\n")
  }

  if (is_final_pgm) {
    if (length(keep_vars) > 3) {
      keep_vars <- paste(keep_vars, collapse = ",\n")
    }
  } else {
    keep_vars <- NA_character_
  }

  return(
    list(
      self = .self,
      has_row_order = any(!is.null(row_order_vars) &
                            !is.na(row_order_vars)),
      row_order_vars = row_order_vars,
      has_keep_vars = any(!is.null(keep_vars) & !is.na(keep_vars)),
      keep_vars = keep_vars
    )
  )
}
