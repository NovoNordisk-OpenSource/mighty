#' Update program order with nodes to load external data
#'
#' @param program_order
#' @param nodes
#'
#' @return
#'
#' @examples
add_nodes_to_load_external_data <- function(program_order, nodes) {

  external_deps <- external_dependencies_per_program(program_order, nodes)
  x <- split(program_order, by = "program_id")

  new_nodes_list <- purrr::imap(x, function(i, nm) {
    deps_i <- external_deps[[nm]]
    if (is.null(deps_i))
      return(i)
    domain_i <- unique(i$domain)
    new_node <- data.table::data.table(matrix(ncol = ncol(i))) |> setnames(names(i))
    new_node[, `:=`(
      domain = domain_i,
      node_id = "external",
      program_id = i$program_id[1],
      rank =
        i[type=="domain_init"|type=="program_init", rank]-0.5,
      type = "external",
      external_dependencies_by_program = list(deps_i)
    )]

  }) |>
    rbindlist()
  prog_order_tmp <- copy(program_order)
  prog_order_tmp[, external_dependencies_by_program := list(NA_character_)]
  out <- rbindlist(list(prog_order_tmp, new_nodes_list)) |>
    setorder(program_id, rank)
  out[, rank := .I]
}



#' Get external dependencies per program
#'
#' @param program_order
#' @param nodes
#'
#' @return
#'
#' @examples
external_dependencies_per_program <- function(program_order, nodes) {
  x <- program_order[nodes[, .(node_id, depend_cols_ext)], on = .(node_id)] |>
    setorder(program_id, rank)
  nodes_by_domain <- split(x[, .(node_id, depend_cols_ext, program_id)], by =
                             "program_id")
  lapply(nodes_by_domain, function(i) {
    i[, rbindlist(depend_cols_ext), by = node_id]
  })

}
