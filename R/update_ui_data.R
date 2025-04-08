#' Update UI data
#'
#' @param payload
#' @param ui_data
#'
#' @return
#' @export
#'
#' @examples
update_ui_data <- function(payload, ui_data) {
  for (name in names(payload)) {
    ui_data <- updata_ui_data_i(ui_data, payload, name)
  }
  return(ui_data)
}


updata_ui_data_i <- function(ui_data, payload, name) {
  nodes <- ui_data$nodes
  action_name <- purrr::imap(nodes, function(i, domain) {
    purrr::imap(i, function(ii, inx) {
      # All function names ins std prog should be in lower case.
      if (!is.na(ii$code_id) &&
          ii$code_id == name)
        return(c(domain, inx))
    })

  })  |> unlist()
  if (is.null(action_name)) {
    return(ui_data)
  }
  list_to_modify <- ui_data$nodes[[action_name[1]]][[action_name[2]]]
  list_to_modify <- modifyList(list_to_modify, payload[[name]])
  ui_data$nodes[[action_name[1]]][[action_name[2]]] <- list_to_modify

  return(ui_data)
}



update_ui_data_2 <- function(payload, ui_data) {
  tmp <- convert_nested_list_to_dt(ui_data) |>
    merge_rows_with_same_code_id()

  metadata_from_active_code_ids <- payload[tmp[!is.na(code_id),code_id]]
  metadata_from_active_code_ids_transposed <- purrr::list_transpose(metadata_from_active_code_ids)
  code_id_data <- data.table(code_id=names(metadata_from_active_code_ids),
             type = metadata_from_active_code_ids_transposed$type,
             depend_cols=metadata_from_active_code_ids_transposed$depend_cols,
             outputs = metadata_from_active_code_ids_transposed$outputs)

# TODO: check if the outputs described by data match the columns specified in
# the adam specs. For new, we just use the metadata supplied by the standard
# components
x <- merge(tmp, code_id_data, by="code_id", suffixes = c("", "_from_code"), all.x = TRUE)
x[!is.na(code_id),`:=` (depend_cols=depend_cols_from_code, type = type_from_code, outputs = outputs_from_code),]
x$outputs_from_code <- x$type_from_code <- x$depend_cols_from_code <- x$column <- NULL

# For each depend_cols entry, add attribute information detailing:
# - domain
# - domain type
x[,depend_cols:= lapply(depend_cols, domain_column_decorator)]
}

convert_nested_list_to_dt <- function(nested_list) {
  parent_names <- names(nested_list)

  # Initialize empty lists to store each column
  type_list <- vector("character", length(parent_names))
  origin_list <- vector("list", length(parent_names))
  depend_cols_list <- vector("list", length(parent_names))
  outputs_list <- vector("list", length(parent_names))
  code_id_chr <- vector("character", length(parent_names))
  depend_rows_list <- vector("character", length(parent_names))
  parent_names_chr <- vector("character", length(parent_names))

  # Extract data from each parent element
  for (i in seq_along(parent_names)) {
    parent <- parent_names[i]
    parent_data <- nested_list[[parent]]
    parent_names_chr[[i]] <- list(parent_names[i])
    type_list[[i]] <- if (!is.null(parent_data$type))
      parent_data$type
    else
      NA_character_
    origin_list[[i]] <- if (!is.null(parent_data$origin))
      parent_data$origin
    else
      NA_character_
    depend_cols_list[[i]] <- if (!is.null(parent_data$depend_cols))
      list(parent_data$depend_cols)
    else
      list(NA)
    outputs_list[[i]] <- if (!is.null(parent_data$outputs))
      list(parent_data$outputs)
    else
      list(NA)
    depend_rows_list[[i]] <- if (!is.null(parent_data$depend_rows))
      list(parent_data$depend_rows)
    else
      list(NA)
    code_id_chr[[i]] <- if (!is.null(parent_data$code_id))
      parent_data$code_id
    else
      NA_character_
  }


  dt <- data.table(
    column = unlist(parent_names_chr, recursive = FALSE),
    type = type_list,
    origin = origin_list,
    depend_cols = unlist(depend_cols_list, recursive = FALSE),
    outputs = unlist(outputs_list, recursive = FALSE),
    depend_rows = unlist(depend_rows_list, recursive = FALSE),
    code_id = code_id_chr

  )

  return(dt)
}

merge_rows_with_same_code_id <- function(x) {
  result <- x[!is.na(code_id), .(
    column = column |> unlist() |> unique() |> list(),
    type = unique(type),
    origin = origin |> unlist() |> unique() |> list(),
    depend_cols = origin |> unlist() |> unique() |> list(),
    outputs = outputs |> unlist() |> unique() |> list(),
    depend_rows = depend_rows |> unlist() |> unique() |> list()
  ), by = code_id]

  list(x[is.na(code_id)], result) |>
    rbindlist(fill = TRUE)
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
