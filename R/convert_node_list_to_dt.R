#' Convert node list to data.table
#'
#' @param nodes
#'
#' @return
#' @export
#' @import data.table
#' @examples
convert_node_list_to_dt <- function(nodes){

  nodes_2 <- nodes |>
    replace_self_with_domain() |>
    flatten_top_two_levels() |>
    add_attributes()
  purrr::imap(nodes_2, function(node_i, nm){

    data.table::data.table(
      node_id = nm,
      domain = node_i$domain,
      action = node_i$action,
      code_id = node_i$code_id,
      type = node_i$type,
      depend_cols = list(node_i$depend_cols),
      depend_rows = list(node_i$depend_rows),
      parameters = list(node_i$parameters),
      origin = node_i$origin,
      outputs = list(node_i$outputs),
      depend_cols_complete = list(node_i$depend_cols_complete),
      outputs_complete = list(node_i$outputs_complete)
    )
  }) |> data.table::rbindlist(fill = TRUE)

}
