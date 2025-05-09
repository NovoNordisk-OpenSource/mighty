assert_all_parents_present <- function(x){

  x_no_rows <- x[(is.na(type)|type!="row")]
  dependencies <- purrr::map2(x$domain, x$depend_cols, function(domain_table, depend_col){
    from_adam_domains <- c()
    has_adam_dependencies <- depend_col[grepl("^AD", domain, ignore.case = TRUE), nrow(.SD)] > 0
    if(has_adam_dependencies){
      from_adam_domains <- depend_col[grepl("^AD", domain, ignore.case = TRUE), paste0(domain, ".", column_name)]
    }
    from_adam_domains
  }) |> unlist()
  outputs <- purrr::map2(x_no_rows$domain, x_no_rows$outputs, function(domain, output){
    paste0(domain, ".", unlist(output))
  }) |> unlist()

  missing_parents <- setdiff(dependencies, outputs)
  if(length(missing_parents)>0){

    stop("\n\n The following columns are parents of other columns, but are not in the ADaM spec:\n", paste0(missing_parents, collapse = "\n"))
  }

}
