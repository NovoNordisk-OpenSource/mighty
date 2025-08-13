#' Render Code for Actions
#' @description Processes a data table of actions to generate executable code
#'   for each action by determining appropriate parameters and rendering the
#'   corresponding code components.
#'
#' @details For each action in the input table, this function:
#'   1. Calls define_params() to determine the correct parameters based on the code_id
#'   2. Renders the code component using the mighty.standards library
#'   3. Adds a node header and stores the generated code back in the actions table
#'
#' @param actions Data table containing action definitions with columns including
#'   code_id, domain, outputs, depend_cols, parameters, and node_id
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns
#' @param ui_data List containing domain-specific UI data including initialization
#'   metadata
#' @param path_trial Character string specifying the output path where
#'   generated programs and data should be stored
#'
#' @returns The input actions data table with an additional 'code' column
#'   containing the generated code for each action
render_code <- function(
  actions,
  domain_keys,
  ui_data,
  path_trial
) {
  actions_ <- copy(actions)
  for (i in seq_len(nrow(actions_))) {
    action_i <- actions_[i]

    params <- define_params(
      code_id = action_i$code_id,
      .self = action_i$domain,
      output_cols = action_i$outputs[[1]],
      depend_domains = action_i$depend_cols[[1]]$domain,
      depend_columns = action_i$depend_cols[[1]]$column_name,
      action_parameters = action_i$parameters,
      node_id = action_i$node_id,
      path_trial = path_trial,
      init_metadata = ui_data[[action_i$domain]]$init,
      domain_keys = domain_keys
    )

    code_i <- paste0(
      gen_node_header(action_i$node_id),
      mighty.standards::get_rendered_component(component = 
        action_i$code_id |> format_internal_code_id(),
        params = params
      )$code |>
        paste0(collapse = "\n")
    )

    actions_[i, code := code_i]
  }
  actions_
}

define_params <- function(
  code_id,
  .self,
  output_cols,
  depend_domains,
  depend_columns,
  action_parameters,
  node_id,
  path_trial,
  init_metadata,
  domain_keys
) {
  
  switch(
    code_id,
    "_read_data.mustache" = params_read_data_code(
      payload = output_cols,
      domain = .self,
      path_trial = path_trial
    ),
    "_init_domain.mustache" = params_init_domain_code(
      .self = .self,
      keep_columns = output_cols,
      source_domains = depend_domains |> unique()
    ),
    "_filter_domain.mustache" = params_domain_filter_code(
      .self = .self,
      init_metadata = init_metadata,
      source_domains = depend_domains |>
        unique(),
      keep_columns = output_cols,
      domain_keys = domain_keys
    ),
    "_col_mutate.mustache" = params_mutate_code(
      .self = .self,
      rename_var = output_cols,
      source_var = depend_columns,
      node_id = node_id
    ),
    "_col_echo.mustache" = params_col_echo_code(
      .self = .self,
      depend_cols = depend_columns,
      depend_domains = depend_domains,
      outputs = output_cols,
      domain_keys = domain_keys
    ),
    "_write_data.mustache" = params_write_domain_code(.self = .self),
    # Default case for col_compute/row_compute 
    format_col_compute_params(action_parameters = action_parameters)
  )
}

get_internal_component_path <- function(code_id) {
  system.file("components", paste0(code_id, ".mustache"), package = "mighty")
}

format_col_compute_params <- function(action_parameters) {
  params <- action_parameters |> unlist(recursive = FALSE)
  if (!is.list(params) && is.na(params)) {
    return(list())
  }
  params
}


gen_node_header <- function(node_id) {
  paste0(
    "\n# ",
    node_id,
    "---------------------\n"
  )
}


#' @noRd
#' Needed for mustache components stored in mighty. These code ids need to be transformed into
#' a relative path to where the components are located so that get_rendered_component will
#'  know where to source the files from
format_internal_code_id <- function(code_id) {
  if(!startsWith(code_id, "_")){
    return(code_id)
  }
  file.path("components", code_id) |> 
      system.file(package = "mighty")
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
