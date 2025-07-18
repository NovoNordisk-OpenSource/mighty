params_write_domain_code <- function(.self, keep_columns = NULL) {
  return(list(
    self_upper = toupper(.self),
    self = .self,
    keep_cols = keep_columns
  )
  )
}
