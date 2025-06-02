assign_predecessor_action_types <- function(nodes) {

  x <- copy(nodes)

  # Extract domain of dependency columns
  dep_domain <- extract_domain_of_dependency_columns(x)

  # Identify indices of of col_copy nodes and col_mutate nodes
  index_copy_mutate <- which(is.na(x[["code_id"]]) & dep_domain == "core")

  # Assign action type "col_echo"
  index_echos <- x[, code_id] |>
    is.na() |>
    which() |>
    setdiff(index_copy_mutate)
  x[index_echos, type := "col_echo"]

  # Return early when empty
  if (length(index_copy_mutate) == 0) {
    return(x)
  }

  # Assign action types "col_copy" and "col_mutate"
  # We need to distinguish between col_copy and col_mutate nodes, because downstream
  # col_copy nodes will be absorbed my the domain_init nodes, but mutates nodes will
  # not
  mutate_node_ids <- extract_mutate_node_ids(x, index_copy_mutate)
  x[index_copy_mutate, type := "col_copy"]
  x[node_id %in% mutate_node_ids, type := "col_mutate"]

  return(x)
}

extract_mutate_node_ids <- function(x, index_copy_mutate) {
  copy_mutate_nodes <- x[index_copy_mutate]
  copy_mutate_depend_cols <- vapply(copy_mutate_nodes$depend_cols,
                                    `[[`,
                                    "column_name",
                                    FUN.VALUE = character(1L))
  copy_mutate_output_cols <- copy_mutate_nodes$output |> unlist()

  return(copy_mutate_nodes[copy_mutate_depend_cols != copy_mutate_output_cols, node_id])
}



extract_domain_of_dependency_columns <- function(x) {
  x$depend_cols |>
    lapply(function(i) {
      if (nrow(i) > 1) {
        # Ignore if multiple dependencies are present in which case the action is
        # not a col_copy, col_mutate, or col_echo, and needs no update
        return("")
      }
      i$domain
    }) |>
    unlist()
}
