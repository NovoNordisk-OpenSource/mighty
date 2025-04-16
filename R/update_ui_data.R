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
  metadata_from_active_code_ids <- payload[ui_data[!is.na(code_id), code_id]]

  metadata_from_active_code_ids_transposed <- purrr::list_transpose(metadata_from_active_code_ids)
  code_id_data <- data.table(
    code_id = names(metadata_from_active_code_ids),
    type = metadata_from_active_code_ids_transposed$type,
    depend_cols = metadata_from_active_code_ids_transposed$depend_cols,
    outputs = metadata_from_active_code_ids_transposed$outputs
  )

  # TODO: check if the outputs described by data match the columns specified in
  # the adam specs. For new, we just use the metadata supplied by the standard
  # components


  x <- merge(
    ui_data,
    code_id_data,
    by = "code_id",
    suffixes = c("", "_from_code"),
    all.x = TRUE
  )

  assert_outputs_identical <- function(x) {
    # Only derivations can have discrepancies as they have "column" defined both
    # in yml and in code components
    x_sub <- x[type_from_code == "derivation", .(code_id, outputs, outputs_from_code)]
    inx <- purrr::map2(x_sub$outputs, x_sub$outputs_from_code, function(yml, code) {
      all(yml == code)
    }) |> unlist()

    if(sum(inx)==0) return(TRUE)
    x_error<- x_sub[!inx]
    error_list <- list()
    for(i in seq_len(nrow(x_error))){

      error_list[[x_error[i, code_id]]] <-
        list(Outputs_from_code_component = x_error[i, unlist(outputs_from_code)],
             Outputs_from_specificaton = x_error[i, unlist(outputs)])
    }
    stop(error_list |> pretty_error_outputs())

  }
  assert_outputs_identical(x)
  x[!is.na(code_id), `:=` (depend_cols = depend_cols_from_code,
                           type = type_from_code,
                           outputs = outputs_from_code), ]
  x$outputs_from_code <- x$type_from_code <- x$depend_cols_from_code <- x$column <- NULL



  x[, depend_cols := purrr::map2(depend_cols, domain, depend_cols_nested_data_table)]

  assert_all_parents_present(x)

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
