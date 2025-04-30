convert_yml_to_data_table <- function(yml_list) {

  lapply(yml_list, function(i) {
    convert_yml_to_data_table_(i$columns, i$domain) |>
      merge_rows_with_same_code_id()
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

merge_rows_with_same_code_id <- function(x) {
  # This logic is needed because for columns, a single code_id can be referenced
  # by multiple columns, but each MUST be paramaterized the same - because in
  # essence this code ID can only appear once per ADaM domain in the final ADaM
  # program. In contrast, multiple row nodes can have the same code_id with
  # DIFFERENT parameters.

  non_predecessor_nodes <- x[!is.na(code_id)]
  non_predecessor_nodes[column == "", tmp_id := paste0(code_id, parameters)]
  non_predecessor_nodes[column != "", tmp_id := code_id]

  result <- non_predecessor_nodes[, .(
    column = column |> unlist() |> unique() |> list(),
    type = unique(type),
    domain = unique(domain),
    parameters = collect_parameters(parameters, unique(code_id)),
    depend_cols = depend_cols |> unlist() |> unique() |> list(),
    outputs = outputs |> unlist() |> unique() |> list(),
    depend_rows = depend_rows |> unlist() |> unique() |> list(),
    code_id = unique(code_id)
  ), by = tmp_id]
  result[, tmp_id := NULL]

  list(x[is.na(code_id)], result) |>
    data.table::rbindlist(fill = TRUE)
}


collect_parameters <- function(parameters, code_id) {
  does_not_have_parameters <- all(is.na(parameters))
  if (does_not_have_parameters) {
    return(list(NA_character_))
  }

  # Remove top-layer list
  parameters_2 <- unlist(parameters, recursive = FALSE)

  # If the code_id, or code_id param combo (for rows) only appears once, no
  # check is needed
  only_one_invocation_of_code_id <- length(parameters)==1
  if(only_one_invocation_of_code_id){
    return(list(parameters_2))
  }

  # Create a data frame to combine names and values
  df <- data.frame(
    name = names(parameters_2),
    value = unlist(parameters_2),
    stringsAsFactors = FALSE
  )

  # Get unique combinations of names and values
  unique_entries <- df[!duplicated(df), ]

  # Create a new list from the unique entries
  unique_list <- setNames(as.list(unique_entries$value), unique_entries$name)

  dissimilar_parameters <- length(unique_list) != length(unique(names(unique_list)))
  if (dissimilar_parameters) {
    # `dissimilar_parameters` is only possible to be TRUE for code_id's
    # associated with column operations. For row operations,
    # `dissimilar_parameters` can only be FALSE, because we use the combination
    # of code_id and parameter values to generate the group_by variable,
    dub_param <- unique_entries[["name"]][duplicated(unique_entries["name"])]
    stop(
      paste0(
        "\nCode_id `", code_id, "` is used in multiple columns with different paramenters.\n",
        "If a code_id is used in multiple columns, the parameter values must be the same. \n",
        "This restriction may be relaxed in future versions.\n",
        "For now, if you need the same underlying logic, create multiple functions by writing a thin wrapper around the core code",
        paste(dub_param, collapse = ", ") ,
        "."
      )
    )
  }
  list(unique_list)
}
