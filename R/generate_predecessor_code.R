predecessor_mutate <-  function(.self, rename_var, source_var, node_id) {

  #header <- sub(".*-", "", node_id)
  header <- node_id

  glue::glue('
 # {toupper(header)} -----------------------------------------------------

  {.self} <- {.self} |> dplyr::mutate({toupper(rename_var)} = {toupper(source_var)})

')
}


#' Pre-process variables for predecessor left joins
#' @description Prepares the inputs to the predecessor_left_join function based
#'   on the "nodes" data model
#' @return a list of strings to be used in the predecessor_left_join function
#' @export
#'

pre_process_predecessor_left_join <- function(depend_columns, depend_domains, outputs, domain, domain_keys) {

  # Extract the domain (everything before the first ".")
  depend_domains <- depend_domains |> unique()
  join_dataset <- depend_domains[!grepl(domain, depend_domains)]
  checkmate::assertTRUE(length(join_dataset)==1)
  by_vars <- domain_keys[[join_dataset]]
  if(is.null(by_vars)){
    stop("The domain keys for ", join_dataset, " are not defined")
  }
  # Remove the join variables from the depend_cols
  regexp <- paste0(by_vars, collapse = "|")
  var_to_add <- depend_columns[!grepl(regexp, depend_columns)]

  # Remove all the domain prefixes
  var_to_add <- sub(".*\\.", "", var_to_add)
  return(list(
    join_dataset = join_dataset,
    var_to_add = var_to_add,
    by_vars = by_vars,
    output_var = sub(".*\\.", "", outputs)
  ))
}

predecessor_left_join <- function(.self, join_dataset, var_to_add, by_vars, node_id, output_var) {

  by_vars_str <- paste(sprintf('"%s"', by_vars), collapse = ", ")
  select_expr <- c(by_vars, var_to_add) |>
    unique() |>
    paste0(collapse = ", ")

  header <- node_id

  left_join_code <- glue::glue(
    "
    # {header} -----------------------------------------------------

    {.self} <- {.self} |>
        dplyr::left_join({join_dataset} |> dplyr::select({select_expr}),
    by = c({by_vars_str}))"
  )

  if (var_to_add != output_var){
    left_join_code <- glue::glue("{left_join_code} |>
                                 dplyr::rename({output_var} = {var_to_add})")
  }

  left_join_code <- glue::glue("{left_join_code}

  ")

  return(as.character(left_join_code))
}
