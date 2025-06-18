add_write_domain_nodes <- function(x) {
  tmp <- x[, make_write_domain_node(.SD, .BY), by = program_id]
  rbindlist(list(x, tmp), fill = TRUE) |> setorder(rank, program_id)
}

make_write_domain_node <- function(node_subset, program_id) {
  inx <- node_subset[, max(rank)] + 1
  domain_i <- node_subset[1, domain]
  node_id <- paste0(domain_i, "-", unlist(program_id), "-write_domain")
  data.table::data.table(
    node_id = node_id,
    domain = domain_i,
    type = "write_domain",
    rank = inx,
    input_cols = NA_character_
  )
}
