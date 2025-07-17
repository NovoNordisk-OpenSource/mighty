#' Generate Program Code
#' @description Generates complete program code by processing nodes in specified
#'   order and combining them into executable programs.
#'
#' @details Merges program ordering information with node data, sorts nodes by
#'   program and rank, then generates code for each program by processing nodes
#'   sequentially.
#'
#' @param program_order Data table containing program execution order with
#'   columns for node_id, domain, program_id, rank, type, and external
#'   dependencies
#' @param nodes Data table containing node definitions and specifications
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns
#' @param code_component_env Environment containing code components
#' @param trial_metadata List containing trial-specific metadata including data
#'   paths and connection information
#' @param ui_data List
#' @param path_trial Optional character string specifying the output path where
#'   generated programs and data should be stored.
#'
#' @returns A named list of generated programs where each element contains the
#'   complete code for one program, with names in the format "program_id_domain"
render_code <- function(
  actions,
  domain_keys,
  ui_data,
  path_trial,
  standards_lib
) {
  actions_ <- copy(actions)
  for (i in seq_len(nrow(actions_))) {
    action_i <- actions_[i]

    code_i <- dispatch_code_gen(
      action_i = action_i,
      ui_data = ui_data,
      domain_keys = domain_keys,
      path_trial = path_trial,
      standards_lib = standards_lib
    )

    actions_[i, code := code_i]
  }
  actions_
}

dispatch_code_gen <- function(
  action_i,
  ui_data,
  domain_keys,
  path_trial,
  standards_lib
) {
  params <- action_i$parameters |> unlist(recursive = FALSE)
  if (!is.list(params) && is.na(params)) {
    params <- list()
  }
  switch(
    action_i$code_id,
    "_read_data" = generate_read_data_code(
      payload = action_i$outputs[[1]],
      domain = action_i$domain,
      path_trial = path_trial
    ),
    "_init_domain" = generate_init_domain_code(
      .self = action_i$domain,
      keep_columns = action_i$outputs[[1]],
      source_domains = action_i$depend_cols[[1]]$domain |> unique()
    ),
    "_filter_domain" = generate_domain_filter_code(
      .self = action_i$domain,
      init_metadata = ui_data[[action_i$domain]]$init,
      source_domains = action_i$depend_cols[[1]]$domain |>
        unique(),
      keep_columns = action_i$outputs[[1]],
      domain_keys = domain_keys
    ),
    "_col_mutate" = generate_mutate_code(
      .self = action_i$domain,
      rename_var = action_i$outputs[[1]],
      source_var = action_i$depend_cols[[1]]$column_name,
      node_id = action_i$node_id
    ),
    "_col_echo" = generate_col_echo_code(
      .self = action_i$domain,
      depend_cols = action_i[["depend_cols"]][[1]][["column_name"]],
      depend_domains = action_i[["depend_cols"]][[1]][["domain"]],
      outputs = action_i[["outputs"]][[1]],
      node_id = action_i$node_id,
      domain_keys = domain_keys
    ),
    "_write_data" = generate_write_domain_code(.self = action_i$domain),
    # Default case
    {
      paste0(
        "\n# ",
        action_i$node_id,
        "---------------------\n",
        mighty.standards::get_rendered_component(
          action_i$code_id,
          params = params
        )$code |>
          paste0(collapse = "\n")
      )
    }
  )
}


compile_into_programs <- function(actions) {
  program_blocs <- actions |>
    split(by = "program_id")
  program_names <- program_blocs |>
    vapply(\(x) unique(x$domain), FUN.VALUE = character(1L))
  program_names <- paste0(seq_along(program_names), "_", program_names)

  programs <- program_blocs |>
    lapply(collapse_code) |>
    setNames(program_names)
}

collapse_code <- function(program_i) {
  paste(program_i$code, collapse = "\n")
}
