#' Add program init nodes
#'
#' @param program_order
#' @param nodes_6
#'
#' @return
#' @export
#'
#' @examples
add_program_init_nodes <- function(program_order, nodes_6){
  x <- split(program_order, by = "domain")
  greater_than_1_program_id <- function(i){i$program_id |> unique() |> length() > 1}
  names_of_domains_that_are_split <- Filter(greater_than_1_program_id, x) |>
    names()
  # For each domain that is split, add a program init node for each program that
  # is not the first program for that domain
  program_init_nodes <- lapply(names_of_domains_that_are_split, f, x=x)
  out<- x[!names(x) %in% names_of_domains_that_are_split] |>
    c(program_init_nodes) |>
    rbindlist() |>
    data.table::setorder(rank)
}

f<- function(domain, x){
  program_order_domain <- x[[domain]]
  program_ids <- program_order_domain$program_id |> unique()
  program_ids <- program_ids[program_ids != min(program_ids)]

  # For each unique program_id, add a program init node with a rank that is below the
  # min rank of all nodes in that program
  program_init_nodes_domain <- lapply(program_ids, function(program_id_i){
    program_order_program <- program_order_domain[program_id == program_id_i]
    min_rank <- min(program_order_program$rank)

    program_init_node <- data.table(
      domain = domain,
      node_id = paste0("program_init_", domain, "_", program_id_i),
      node_group = NA,
      program_id = program_id_i,
      rank = min_rank - 0.1,
      type = "program_init"
    )
    program_init_node
  }) |> rbindlist()
  rbindlist(list(program_init_nodes_domain, program_order_domain)) |>
    data.table::setorder(rank)
}
