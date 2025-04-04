#' Convert node list to data.table
#'
#' @param nodes
#'
#' @return
#' @export
#' @import data.table
#' @examples
convert_node_list_to_dt <- function(nodes){

  nodes <- nodes |>
    replace_self_with_domain() |>
    flatten_top_two_levels() |>
    add_attributes()


  purrr::imap(nodes, function(node_i, nm){

    data.table::data.table(
      node_id = nm,
      domain = node_i$domain,
      action = node_i$action,
      code_id = node_i$code_id,
      type = node_i$type,
      depend_rows = list(node_i$depend_rows),
      parameters = list(node_i$parameters),
      origin = node_i$origin,
      depend_cols = list(node_i$depend_cols),
      outputs = list(node_i$outputs)
    )
  }) |> data.table::rbindlist(fill = TRUE)

}
