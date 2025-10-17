add_initialize_domain_actions <- function(actions, ui_init) {
  col_copy_outputs <- split(
    actions[actions$type == "col_copy", c("domain", "outputs")],
    by = "domain"
  )

  has_domain_filter <- lapply(
    purrr::list_transpose(ui_init)$filter_domain,
    function(x) {
      any(!is.na(unlist(x)))
    }
  )

  domain_init_actions <-
    lapply(seq_len(length(col_copy_outputs)), function(i) {
      domain_i <- names(col_copy_outputs)[[i]]

      # The output columns from domain_init consists of all outputs from
      # col_copy actions
      outputs_i <- col_copy_outputs[[i]][["outputs"]] |>
        unlist() |>
        unique()

      # Depend cols is the same as the outputs expanded to each core SDTM domain
      depend_cols_i <- expand.grid(
        column_name = outputs_i,
        domain = ui_init[[domain_i]]$base_domains,
        stringsAsFactors = FALSE
      ) |>
        as.data.table()
      depend_cols_i[["domain_type"]] <- classify_data_domains(
        depend_cols_i$domain
      )

      # Add temporary source domain indicator column if domain filters are
      # present
      if (has_domain_filter[[domain_i]]) {
        outputs_i_final <- c(outputs_i, "SRC_")
      } else {
        outputs_i_final <- outputs_i
      }

      domain_init_action <- data.table(
        node_id = paste0(domain_i, "-init_domain"),
        code_id = "_init_domain.mustache",
        type = "init_domain",
        depend_cols = list(depend_cols_i),
        outputs = list(outputs_i_final),
        depend_rows = list(NA),
        parameters = list(NA),
        domain = domain_i
      )
    }) |>
    rbindlist()

  # Add init_domain actions to set of actions
  actions_updated <- rbind(actions, domain_init_actions)
  return(actions_updated)
}
