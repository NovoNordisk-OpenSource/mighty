#' Title
#'
#' @param payload
#' @param ui_data
#'
#' @returns
#' @export
#'
#' @examples
update_ui_data <- function(code_component_metadata, ui_data) {
  metadata_from_active_code_ids <- code_component_metadata[unique(ui_data[!is.na(code_id), code_id])]

  # If there are any code id references in the ui data then fetch code id metadata and update ui data
  if(length(metadata_from_active_code_ids) > 0) {
    metadata_from_active_code_ids_transposed <- purrr::list_transpose(metadata_from_active_code_ids)
    code_id_data <- data.table::data.table(
      code_id = names(metadata_from_active_code_ids),
      type = metadata_from_active_code_ids_transposed$type,
      depend_cols = metadata_from_active_code_ids_transposed$depend_cols,
      outputs = metadata_from_active_code_ids_transposed$outputs,
      parameters_defaults = metadata_from_active_code_ids_transposed$parameters_defaults
    )

    x <- merge(
      ui_data,
      code_id_data,
      by = "code_id",
      suffixes = c("", "_from_code"),
      all.x = TRUE
    )
    # Temporarily convert all "derivation" components into "compute" to match
    # our internal terminology
    x[type_from_code=="derivation", type_from_code:= "compute"]


    assert_outputs_identical(x)

    # Consolidate columns for data that is redundant
    x[!is.na(code_id), `:=` (depend_cols = depend_cols_from_code,
                             type = type_from_code,
                             outputs = outputs_from_code), ]
    x$outputs_from_code <- x$type_from_code <- x$depend_cols_from_code <- x$column <- NULL

  } else { # if there are no code_id references in the ui data then just use the ui data
    x <- ui_data
  }

  x[, depend_cols := purrr::map2(depend_cols, domain, depend_cols_nested_data_table)]

  return(x)
}


depend_cols_nested_data_table <- function(i, domain_i) {
  result <- vector("list", length(i))
  # Extract domains in one vectorized operation
  elements <- unlist(i)

  inx <- grepl("\\.", elements)
  n_with_dot <- sum(inx)
  if (n_with_dot == 0) {
    return(
      data.table::data.table(
        column_name = elements,
        domain = domain_i,
        domain_type = classify_external_data_domains(domain_i)
      )
    )
  }
  if (n_with_dot == length(elements)) {
    domains <- sub("\\.(.*)", "", elements)
    column <- sub("^[^.]*\\.", "", elements)

    domain_type <- classify_external_data_domains(domains)
    return(
      data.table::data.table(
        column_name = column,
        domain = domains,
        domain_type = domain_type
      )
    )
  }

  elements_with_dot <- elements[inx]
  elements_no_dot <- elements[!inx]
  domains <- sub("\\.(.*)", "", elements_with_dot)
  column <- sub("^[^.]*\\.", "", elements_with_dot)
  domain_type <- classify_external_data_domains(domains)

  list(
    data.table::data.table(
      column_name = column,
      domain = domains,
      domain_type = domain_type
    ),
    data.table::data.table(
      column_name = elements_no_dot,
      domain = domain_i,
      domain_type = classify_external_data_domains(domain_i)
    )
  ) |>
    data.table::rbindlist()

}

add_node_id_fast <- function(nodes) {
  formatted_outputs <- ifelse(!is.na(nodes$outputs), lapply(nodes$outputs, function(x) paste0(unlist(x), collapse = "-")), "")
  formatted_code_id <- ifelse(!is.na(nodes$code_id), paste0("-", nodes$code_id), "")
  formatted_parameters <- ifelse(!is.na(nodes$parameters), paste0("-", nodes$parameters), "")

  nodes$node_id <- paste0(nodes$domain, "-",
                          formatted_outputs,
                          formatted_code_id,
                          formatted_parameters)

  return(nodes)
}
