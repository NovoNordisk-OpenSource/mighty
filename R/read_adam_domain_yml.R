read_adam_specs <- function(paths){
  lapply(paths, read_adam_domain_yml) |>
    unlist(recursive = FALSE)

}


read_adam_domain_yml <- function(yml) {
  x <- yaml::read_yaml(yml)
  # Name elements in the list
  names(x$column_metadata) <- lapply(x$column_metadata, function(i){i$column})
  if(!is.null(x$row_actions)){

    names(x$row_actions) <- lapply(x$row_actions, function(i){i$code_id})
    tmp <- c(x$column_metadata, x$row_actions)
  }
  else{
    tmp <- x$column_metadata
  }
  # Restructure to match internal data model
  out <- lapply(tmp, function(i) {
    # rename the element "source" to "depend_col"
    i$depend_cols <- i$source
    i$outputs <- i$column
    i$source <- i$column <- NULL

    # if elements don't exist, add them with a value of NA
    names_i <- names(i)
    if (!"depend_rows" %in% names_i) {
      i$depend_rows <- NA
    }
    if (!"parameters" %in% names_i) {
      i$parameters <- NA
    }
    return(i)
  })
  return_list <- list(columns = out,
       domain = x$table_metadata$table,
       keys = x$table_metadata$keys,
       init = x$init) |>
  convert_to_NA_character()
  return(setNames(list(return_list), return_list$domain))


}
