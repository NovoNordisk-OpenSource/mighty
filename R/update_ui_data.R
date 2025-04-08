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
x[,depend_cols:= lapply(depend_cols, domain_column_decorator)]
return(x)
}

domain_column_decorator <- function(i){
  result <- vector("list", length(i))
  # Extract domains in one vectorized operation
  elements <- unlist(i)
  domains <- sub("\\.(.*)", "", elements)
  column <- sub("^[^.]*\\.", "", elements)
  domain_type <- classify_external_data_domains(domains)

  for (idx in seq_along(elements)) {
    new_element <- column[idx]
    attr(new_element, "domain") <- domains[idx]
    attr(new_element, "domain_type") <- domain_type[idx]
    result[[idx]] <- new_element
  }
  return(result)
}


add_node_id <- function(nodes){
  for(i in seq_len(nrow(nodes))){
    nodes[i, node_id := paste0(domain, "-", paste0(unlist(outputs),collapse = "-"))]
  }

}
