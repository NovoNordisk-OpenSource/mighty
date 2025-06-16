#' Add initialize_domain nodes
#'
#' @param program_order
#' @param nodes_6
#'
#' @return
#' @export
#'
#' @examples
add_initialize_domain_nodes <- function(program_order, nodes_6){
  x <- split(program_order, by = "domain")

  # For each domain that is split, add a initialize_domain node for each program
  # that is the first program for that domain
  initialize_domain_nodes <- lapply(names(x), add_initialize_domain_node_by_domain, x = x)

  out <- x |>
    c(initialize_domain_nodes) |>
    rbindlist() |>
    data.table::setorder(rank)
}

add_initialize_domain_node_by_domain <- function(domain, x){
  program_order_domain <- x[[domain]]
  program_id <- program_order_domain$program_id |> min()
  program_order_program <- program_order_domain[program_id == program_id]
  min_rank <- min(program_order_program$rank)

  # Create an initialize_domain node with a rank that is below the min rank of
  # all nodes in that program
  initialize_domain_node <- data.table(
    domain = domain,
    node_id = paste0(domain, "-initialize_domain"),
    node_group = NA,
    program_id = program_id,
    rank = min_rank - 0.5,
    type = "initialize_domain"
  )
  return(initialize_domain_node)
}
