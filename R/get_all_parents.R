get_all_parents <- function(ref_id, actions, parents = c()) {
  actions <- actions[!actions$type %in% c("init_domain", "filter_domain")]

  # Get action with specified node_id
  node_row <- actions[actions$node_id == ref_id]

  if (nrow(node_row) == 0) {
    # If node x is not found, return unchanged set of parents
    return(parents)
  }

  # Extract dependencies for the current node
  dependencies <- node_row$depend_cols[[1]]

  # Iterate over each dependency
  for (nm in dependencies$column_name) {
    is_parent <- lapply(actions$outputs, function(x) any(x == nm)) |> unlist()

    # Get parent node name
    parent_row <- actions[is_parent & actions$type != "col_copy"]
    parent_name <- parent_row$node_id
    parents <- c(parents, parent_name)

    # Recursively fetch parents and their outputs of the current parent
    parents <- get_all_parents(parent_name, actions, parents)
  }

  return(parents)
}
