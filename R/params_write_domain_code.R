params_write_domain_code <- function(.self,
                                     is_final_pgm,
                                     domain_keys,
                                     domain_ui_data,
                                     available_data) {
  keep_vars <- names(domain_ui_data$columns)[nchar(names(domain_ui_data$columns)) > 0]
  row_order_vars <- domain_keys[[toupper(.self)]]

  if (length(row_order_vars) > 3) {
    row_order_vars <- paste(row_order_vars, collapse = ",\n")
  }

  if (is_final_pgm) {
    if (length(keep_vars) > 3) {
      available_vars <- keep_vars
      if (!is.null(available_data)){
        available_vars <- available_data$column_name[available_data$domain == .self & !available_data$column_name == "SRC_" ]
      }
      keep_vars_contain_uncommented_code <- !(length(available_vars) == length(keep_vars))
      keep_vars <- paste(ifelse(keep_vars %in% available_vars, 
                                keep_vars, 
                                paste0("# ", keep_vars)), collapse = ",\n")
      if (keep_vars_contain_uncommented_code) {
        # Ensure that the last uncommented variable does not end with a comma
        keep_vars <- fix_comma(keep_vars)
        # Since the first or last variable may be commented, ensure all
        # variables are on a separate line
        keep_vars <- paste0("\n", keep_vars, "\n")
      } 
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


fix_comma <- function(str) {
  lines <- strsplit(str, "\n")[[1]]
  non_commented <- which(!startsWith(trimws(lines), "#") & trimws(lines) != "")
  if (length(non_commented)) {
    lines[max(non_commented)] <- gsub(",$", "", lines[max(non_commented)])
    paste(lines, collapse = "\n")
  }
  else
    str
  }
