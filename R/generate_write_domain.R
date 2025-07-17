generate_write_domain_code <- function(.self, keep_columns = NULL) {
  template <- '
  # Write {{self_upper}} to disk ------------------------------------------------
{{#keep_var}}
cnt$adam$write_cnt({{self}} |> dplyr::select({{keep_var}}), "{{self}}.parquet", overwrite = TRUE)
{{/keep_var}}
{{^keep_var}}
cnt$adam$write_cnt({{self}}, "{{self}}.parquet", overwrite = TRUE)
{{/keep_var}}
'

  data <- list(
    self_upper = toupper(.self),
    self = .self,
    keep_cols = keep_columns
  )

  whisker::whisker.render(template, data)

}
