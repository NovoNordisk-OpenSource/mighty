convert_yml_to_data_table <- function(yml_list) {

  lapply(yml_list, function(i) {
    convert_yml_to_data_table_(i$columns, i$domain)
  }) |>
    data.table::rbindlist()
}
convert_yml_to_data_table_ <- function(nested_list, domain) {
  parent_names <- names(nested_list)

  # Initialize empty lists to store each column
  type_list <- vector("character", length(parent_names))
  depend_cols_list <- vector("list", length(parent_names))
  outputs_list <- vector("list", length(parent_names))
  code_id_chr <- vector("character", length(parent_names))
  parameters <- vector("character", length(parent_names))
  depend_rows_list <- vector("character", length(parent_names))
  parent_names_chr <- vector("character", length(parent_names))


  # Extract data from each parent element
  for (i in seq_along(parent_names)) {
    parent_data <- nested_list[[i]]
    parent_names_chr[[i]] <- list(parent_names[i])
    type_list[[i]] <- if (!is.null(parent_data$type))
      parent_data$type
    else
      NA_character_
    depend_cols_list[[i]] <- if (!is.null(parent_data$depend_cols))
      list(parent_data$depend_cols)
    else
      list(NA_character_)
    parameters[[i]] <- if (all(!is.na(parent_data$parameters)))
      list(as.list(unlist(parent_data$parameters)))
    else
      list(NA_character_)
    outputs_list[[i]] <- if (!is.null(parent_data$outputs))
      list(parent_data$outputs)
    else
      list(NA_character_)
    depend_rows_list[[i]] <- if (!is.null(parent_data$depend_rows))
      list(parent_data$depend_rows)
    else
      list(NA_character_)
    code_id_chr[[i]] <- if (!is.null(parent_data$code_id))
      parent_data$code_id
    else
      NA_character_


  }

  dt <- data.table::data.table(
    column = unlist(parent_names_chr, recursive = FALSE),
    type = type_list,
    depend_cols = unlist(depend_cols_list, recursive = FALSE),
    outputs = unlist(outputs_list, recursive = FALSE),
    depend_rows = unlist(depend_rows_list, recursive = FALSE),
    parameters = unlist(parameters, recursive = FALSE),
    code_id = code_id_chr,
    domain = rep(domain, length(parent_names))

  )

  return(dt)
}
