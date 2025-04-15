#' Title
#'
#' @param payload
#' @param ui_data
#'
#' @returns
#' @export
#'
#' @examples
update_ui_data <- function(payload, ui_data) {
  metadata_from_active_code_ids <- payload[ui_data[!is.na(code_id),code_id]]

  metadata_from_active_code_ids_transposed <- purrr::list_transpose(metadata_from_active_code_ids)
  code_id_data <- data.table(code_id=names(metadata_from_active_code_ids),
             type = metadata_from_active_code_ids_transposed$type,
             depend_cols=metadata_from_active_code_ids_transposed$depend_cols,
             outputs = metadata_from_active_code_ids_transposed$outputs)

# TODO: check if the outputs described by data match the columns specified in
# the adam specs. For new, we just use the metadata supplied by the standard
# components

x <- merge(ui_data, code_id_data, by="code_id", suffixes = c("", "_from_code"), all.x = TRUE)
x[!is.na(code_id),`:=` (depend_cols=depend_cols_from_code, type = type_from_code, outputs = outputs_from_code),]
x$outputs_from_code <- x$type_from_code <- x$depend_cols_from_code <- x$column <- NULL


# For each depend_cols entry, add attribute information detailing:
# - domain
# - domain type

x[,depend_cols:= purrr::map2(depend_cols, domain, depend_cols_nested_data_table)]


# TODO: Check for any missing dependencies and connect them to the problematic
# columns
dependencies <- purrr::map2(x$domain, x$depend_cols, function(domain, depend_col){
  paste0(domain, ".", unlist(depend_col$column_name))
}) |> unlist()
outputs <- purrr::map2(x$domain, x$outputs, function(domain, output){
  paste0(domain, ".", unlist(output))
}) |> unlist()

missing_parents <- setdiff(dependencies, outputs)
if(length(missing_parents)>0){
  browser()
  purrr::pmap(list(x$domain,x$depend_cols, x$outputs), function(a,b,d){if("LBTEST" %in% b$column_name) browser()})
  stop("\n\n The following columns are parents of other columns, but are not in the ADaM spec:\n", paste0(missing_parents, collapse = "\n"))
}

return(x)
}


depend_cols_nested_data_table <- function(i, domain_i){
  result <- vector("list", length(i))
  # Extract domains in one vectorized operation
  elements <- unlist(i)
  inx <- grepl("\\.", elements)
  n_with_dot <- sum(inx)
  if(n_with_dot==0){
    data.table::data.table(column_name = elements,
                           domain = domain_i,
                           domain_type = classify_external_data_domains(domain_i))
  }
  if(n_with_dot == length(elements)){
  domains <- sub("\\.(.*)", "", elements)
  column <- sub("^[^.]*\\.", "", elements)

  domain_type <- classify_external_data_domains(domains)
  data.table::data.table(column_name = column,
                         domain = domains,
                         domain_type = domain_type)
  }

  elements_with_dot <- elements[inx]
  elements_no_dot <- elements[!inx]
  domains <- sub("\\.(.*)", "", elements_with_dot)
  column <- sub("^[^.]*\\.", "", elements_with_dot)
  domain_type <- classify_external_data_domains(domains)

  list(data.table::data.table(column_name = column,
                         domain = domains,
                         domain_type = domain_type),
  data.table::data.table(column_name = elements_no_dot,
                         domain = domain_i,
                         domain_type = classify_external_data_domains(domain_i))) |>
    data.table::rbindlist()

}



add_node_id <- function(nodes) {
  for (i in seq_len(nrow(nodes))) {
    nodes[i, node_id := paste0(
      domain,
      "-",
      ifelse(!is.na(outputs), paste0(unlist(outputs), collapse = "-")),
      ifelse(!is.na(code_id), paste0("-", code_id), ""),
      ifelse(!is.na(parameters), paste0("-", parameters), "")
    )]
  }

  return(nodes)
}
