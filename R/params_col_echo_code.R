params_col_echo_code <- function(
  .self,
  depend_cols,
  depend_domains,
  outputs,
  domain_keys
) {
  join_specs <- pre_process_generate_rename_left_join_code(
    depend_cols,
    depend_domains,
    outputs,
    .self,
    domain_keys
  )

  return(list(
    self = .self,
    join_dataset = join_specs$join_dataset,
    select_expr = paste(
      c(join_specs$by_vars, join_specs$var_to_add) |> unique(),
      collapse = ", "
    ),
    by_vars_str = paste(sprintf('"%s"', join_specs$by_vars), collapse = ", "),
    needs_rename = join_specs$var_to_add != join_specs$output_var,
    output_var = join_specs$output_var,
    var_to_add = join_specs$var_to_add
  ))
}

pre_process_generate_rename_left_join_code <- function(
  depend_columns,
  depend_domains,
  outputs,
  domain,
  domain_keys
) {
  depend_domains <- depend_domains |> unique()
  join_dataset <- depend_domains[!grepl(domain, depend_domains)]
  checkmate::assertTRUE(length(join_dataset) == 1)
  by_vars <- domain_keys[[toupper(join_dataset)]]
  if (is.null(by_vars)) {
    stop("The domain keys for ", join_dataset, " are not defined")
  }
  # Remove the join variables from the depend_cols
  var_to_add <- setdiff(depend_columns, by_vars)

  # Remove all the domain prefixes
  var_to_add <- extract_dependency_id(var_to_add)
  return(list(
    join_dataset = join_dataset,
    var_to_add = var_to_add,
    by_vars = by_vars,
    output_var = extract_dependency_id(outputs)
  ))
}
