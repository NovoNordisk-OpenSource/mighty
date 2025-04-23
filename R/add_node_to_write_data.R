add_node_to_write_data <- function(x) {
  tmp <- x[, make_write_data_node(.SD, .BY), by = program_id]
  rbindlist(list(x, tmp), fill = TRUE) |> setorder(rank, program_id)
}

make_write_data_node <- function(node_subset, program_id) {
  inx <- node_subset[, max(rank)] + 1
  domain_i <- node_subset[1, domain]
  node_id <- paste0(domain_i, "-", unlist(program_id), "-write_data")
  data.table::data.table(
    node_id = node_id,
    domain = domain_i,
    type = "write_data",
    rank = inx,
    external_dependencies_by_program = NA_character_
  )
}
