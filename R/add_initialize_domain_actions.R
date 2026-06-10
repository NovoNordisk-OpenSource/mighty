add_initialize_domain_actions <- function(actions, ui_init, filter_domain) {
  col_copy_outputs <- split(
    actions[actions$type == "col_copy", c("domain", "outputs")],
    by = "domain"
  )
  has_domain_filter <- has_domain_level_filter(filter_domain)

  domain_init_actions <- lapply(
    seq_along(col_copy_outputs),
    \(i) {
      build_domain_init_action(
        i,
        col_copy_outputs,
        actions,
        ui_init,
        has_domain_filter
      )
    }
  ) |>
    rbindlist()

  rbind(actions, domain_init_actions)
}

build_domain_init_action <- function(
  i,
  col_copy_outputs,
  actions,
  ui_init,
  has_domain_filter
) {
  domain_i <- names(col_copy_outputs)[[i]]

  # The output columns from domain_init consists of all outputs from col_copy actions
  outputs_i <- col_copy_outputs[[i]][["outputs"]] |>
    unlist() |>
    unique()

  # col_rename source cols that come from SDTM (not ADaM-derived) must be
  # declared as init_domain outputs so the dependency graph can connect
  # col_rename nodes back to init_domain
  adam_derived_cols <- actions[actions$domain == domain_i, ]$outputs |>
    unlist() |>
    unique()
  col_rename_actions_i <- actions[
    actions$type == "col_rename" & actions$domain == domain_i,
  ]
  if (nrow(col_rename_actions_i) > 0) {
    rename_source_cols <- lapply(
      col_rename_actions_i$depend_cols,
      function(dc) dc$column_name
    ) |>
      unlist() |>
      unique()
    sdtm_rename_source_cols <- setdiff(rename_source_cols, adam_derived_cols)
    outputs_i <- c(outputs_i, sdtm_rename_source_cols) |> unique()
  }

  # Depend cols is the same as the outputs expanded to each core SDTM domain
  depend_cols_i <- expand.grid(
    column_name = outputs_i,
    domain = ui_init[[domain_i]]$base_domains,
    stringsAsFactors = FALSE
  ) |>
    as.data.table()
  depend_cols_i[["domain_type"]] <- classify_data_domains(depend_cols_i$domain)

  # Add temporary source domain indicator column if domain filters are present
  outputs_i_final <- if (has_domain_filter[[domain_i]]) {
    c(outputs_i, "SRC_")
  } else {
    outputs_i
  }

  data.table(
    node_id = paste0(domain_i, "-init_domain"),
    code_id = "mighty_init_domain",
    type = "init_domain",
    depend_cols = list(depend_cols_i),
    outputs = list(outputs_i_final),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = domain_i
  )
}
