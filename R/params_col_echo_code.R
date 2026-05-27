#' Build parameters for the `_col_echo` template
#'
#' @param .self Character. The name of the primary dataset being modified.
#' @param depend_cols Character vector. Dependency column names, potentially
#'   domain-prefixed (e.g. `"ADSL.SEX"`).
#' @param depend_domains Character vector. Domains the column depends on.
#' @param outputs Character. The desired output column name in `.self`.
#' @param domain_keys Named list. Primary keys for each domain.
#'
#' @return A named list with `self`, `join_dataset`, `select_expr`, `by_vars`,
#'   `needs_rename`, `output_var`, `var_to_add`.
#' @noRd
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
    by_vars = paste(sprintf('"%s"', join_specs$by_vars), collapse = ", "),
    needs_rename = join_specs$var_to_add != join_specs$output_var,
    output_var = join_specs$output_var,
    var_to_add = join_specs$var_to_add
  ))
}

#' Extract join specs needed to bring a cross-domain column into the current domain
#'
#' @param depend_columns Character vector. Dependency column names.
#' @param depend_domains Character vector. Domains those columns belong to.
#' @param outputs Character. The desired output column name.
#' @param domain Character. The name of the current (target) domain.
#' @param domain_keys Named list. Primary keys for each domain.
#'
#' @return A named list with `join_dataset`, `var_to_add`, `by_vars`,
#'   `output_var`.
#' @noRd
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
    keys_msg <- paste0(
      "Domain keys for ",
      format_domain(join_dataset),
      " are not defined"
    )

    throw_validation_error(
      category = "Missing domain keys",
      details = keys_msg,
      suggestions = c(
        "Add key definitions to {.file _mighty.yml} {.field /external_data} for this domain",
        "Ensure {.field external_data} includes all referenced domains",
        "Check domain name spelling matches specification file"
      )
    )
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
