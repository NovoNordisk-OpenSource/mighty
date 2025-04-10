#' #' Convert node list to data.table
#' #'
#' #' @param nodes
#' #'
#' #' @return
#' #' @export
#' #' @import data.table
#' #' @examples
#' convert_node_list_to_dt <- function(nodes){
#'
#' <<<<<<< HEAD
#'   nodes_2 <- nodes |>
#'     replace_self_with_domain() |>
#'     flatten_top_two_levels() |>
#'     add_attributes()
#'   purrr::imap(nodes_2, function(node_i, nm){
#' =======
#'   nodes_flat <- nodes |>
#'     replace_self_with_domain() |>
#'     flatten_top_two_levels() |>
#'     add_attributes()
#'
#'
#'   purrr::imap(nodes_flat, function(node_i, nm){
#' >>>>>>> feature/simplify_data_model
#'
#'     data.table::data.table(
#'       node_id = nm,
#'       domain = node_i$domain,
#'       code_id = node_i$code_id,
#'       type = node_i$type,
#'       depend_rows = list(node_i$depend_rows),
#'       parameters = list(node_i$parameters),
#'       origin = node_i$origin,
#'       depend_cols = list(node_i$depend_cols),
#'       outputs = list(node_i$outputs)
#'     )
#'   }) |> data.table::rbindlist(fill = TRUE)
#'
#' }
