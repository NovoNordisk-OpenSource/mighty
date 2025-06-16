#' Add read_domain nodes
#'
#' @param program_order
#' @param nodes_6
#'
#' @return
#' @export
#'
#' @examples
add_read_domain_nodes <- function(program_order, nodes_6){
  x <- split(program_order, by = "domain")
  greater_than_1_program_id <- function(i){i$program_id |> unique() |> length() > 1}
  names_of_domains_that_are_split <- Filter(greater_than_1_program_id, x) |>
    names()
  # For each domain that is split, add a read_domain node for each program that
  # is not the first program for that domain
  read_domain_nodes <- lapply(names_of_domains_that_are_split, f, x = x)
  out <- x[!names(x) %in% names_of_domains_that_are_split] |>
    c(read_domain_nodes) |>
    rbindlist() |>
    data.table::setorder(rank)
}

f <- function(domain, x){
  program_order_domain <- x[[domain]]
  program_ids <- program_order_domain$program_id |> unique()
  program_ids <- program_ids[program_ids != min(program_ids)]

  # For each unique program_id, add a program init node with a rank that is below the
  # min rank of all nodes in that program
  read_domain_nodes_domain <- lapply(program_ids, function(program_id_i){
    program_order_program <- program_order_domain[program_id == program_id_i]
    min_rank <- min(program_order_program$rank)

    read_domain_node <- data.table(
      domain = domain,
      node_id = paste0(domain, "-", program_id_i, "-read_domain"),
      node_group = NA,
      program_id = program_id_i,
      rank = min_rank - 0.5,
      type = "read_domain"
    )
    return(read_domain_node)
  }) |> rbindlist()
  rbindlist(list(read_domain_nodes_domain, program_order_domain)) |>
    data.table::setorder(rank)
}
