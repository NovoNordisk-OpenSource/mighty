extract_actions <- function(ui_table) {
  # Create a temporary flat string from named list 'parameter'. This will be
  # used for grouping
  ui_table$parameters_flat <- apply(ui_table, 1, function(row) {
    named_vector <- row[["parameters"]]
    if (any(!is.na(named_vector))) {
      paste(names(named_vector), ":", named_vector, collapse = ", ")
    } else {
      NA_character_
    }
  })

  # Group actions with code components by unique applications of code components
  actions_with_code_id <- NULL
  if (any(!is.na(ui_table$code_id))) {
    actions_with_code_id <- ui_table |>
      dplyr::filter(!is.na(code_id)) |>
      dplyr::group_by(domain, code_id, parameters_flat) |>
      dplyr::summarise(
        outputs = list(unlist(outputs)),
        depend_cols = unique(depend_cols),
        depend_rows = unique(depend_rows),
        parameters = unique(parameters),
        id = unique(id)
      ) |>
      dplyr::ungroup() |>
      dplyr::select(-parameters_flat)
  }

  # Extract residual actions with no code components
  actions_no_code_id <- NULL
  if (any(is.na(ui_table$code_id))) {
    actions_no_code_id <- ui_table |>
      dplyr::filter(is.na(code_id)) |>
      dplyr::select(
        domain,
        code_id,
        parameters,
        outputs,
        id,
        depend_cols,
        depend_rows
      )
  }

  # Collect all actions and add action id
  actions_combined <- dplyr::bind_rows(
    actions_with_code_id,
    actions_no_code_id
  ) |>
    as.data.table() |>
    add_node_id() |>
    update_depend_rows() |>
    dplyr::select(-id)

  return(actions_combined)
}
