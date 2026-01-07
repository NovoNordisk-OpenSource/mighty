#' Render Code for Actions
#'
#' @description
#' Processes a data table of actions to generate executable code for each action
#' by determining appropriate parameters and rendering the corresponding code
#' components.
#'
#' @details
#' For each action in the input table, this function:
#' 1. Calls define_params() to determine the correct parameters based on the code_id
#' 2. Renders the code component using the mighty.component library
#' 3. Adds a node header and stores the generated code back in the actions table
#' 4. Handles special cases for actions with missing data or execution issues
#' 5. Comments out code for actions that cannot execute due to missing dependencies
#'
#' @param actions Data table containing action definitions with columns including
#'   code_id, domain, outputs, depend_cols, parameters, node_id, lineage, and
#'   can_execute.
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns for data operations.
#' @param ui_data List containing domain-specific UI data including initialization
#'   metadata and configuration parameters.
#' @param path_trial Character string specifying the output path where
#'   generated programs and data should be stored.
#' @param available_data Optional parameter containing information about
#'   available data sources, defaults to NULL.
#'
#' @return
#' The input actions data table with an additional 'code' column containing
#' the generated code for each action, properly formatted with headers and
#' comments as needed.
#' @noRd
render_code <- function(
  actions,
  domain_keys,
  ui_data,
  path_trial,
  available_data = NULL
) {
  actions_program_summary <- actions |>
    dplyr::group_by(domain) |>
    dplyr::summarize(last_program = max(program_id))
  final_programs_lkp <- actions_program_summary$last_program
  names(final_programs_lkp) <- actions_program_summary$domain

  actions_ <- copy(actions)

  for (i in seq_len(nrow(actions_))) {
    action_i <- actions_[i]
    action_has_comment <- !is.null(action_i$lineage) &&
      nchar(action_i$lineage) > 0
    no_line_break <- i == 1 && !action_has_comment
    is_final_pgm <- action_i$program_id == final_programs_lkp[[action_i$domain]]
    # In case the call is based on actions that are modified due to missing
    # data, there may be situations where no outputs can be created. In this
    # case, use the removed_outputs to generate code instead
    # TODO: Assess if a similar approach is needed for depend_columns
    output_cols <- action_i$outputs[[1]]
    if (
      length(action_i$outputs[[1]]) == 0 && !is.null(action_i$removed_outputs)
    ) {
      output_cols <- action_i$removed_outputs[[1]]
    }
    params <- define_params(
      code_id = action_i$code_id,
      .self = action_i$domain,
      output_cols = output_cols,
      depend_domains = action_i$depend_cols[[1]]$domain,
      depend_columns = action_i$depend_cols[[1]]$column_name,
      action_parameters = action_i$parameters,
      node_id = action_i$node_id,
      path_trial = path_trial,
      domain_ui_data = ui_data[[action_i$domain]],
      domain_keys = domain_keys,
      is_final_pgm = is_final_pgm,
      available_data = available_data
    )

    code <- mighty.component::get_rendered_component(
      component = action_i$code_id |> format_internal_code_id(),
      params = params
    )$code |>
      paste0(collapse = "\n")
    code_i <- paste0(
      gen_node_header(action_i$node_id, no_line_break),
      ifelse(action_has_comment, add_hash(action_i$lineage), ""),
      ifelse(action_has_comment, "\n", ""),
      ifelse(action_has_comment && !action_i$can_execute, add_hash(code), code)
    )
    actions_[i, code := code_i]
  }
  actions_
}


#' Define Parameters for Code Generation Templates
#'
#' @description
#' A dispatcher function that generates appropriate parameters for different
#' Mustache code generation templates based on the specified code identifier.
#' This function serves as a central hub for parameter preparation across
#' various data processing operations.
#'
#' @param code_id Character string specifying the template identifier.
#'   Supported values include:
#'   \itemize{
#'     \item `"_read_data.mustache"` - Parameters for data reading operations
#'     \item `"_init_domain.mustache"` - Parameters for domain initialization
#'     \item `"_filter_domain.mustache"` - Parameters for domain filtering
#'     \item `"_col_mutate.mustache"` - Parameters for column mutation
#'     \item `"_col_echo.mustache"` - Parameters for column echoing
#'     \item `"_write_data.mustache"` - Parameters for data writing operations
#'   }
#'   Any other value defaults to column compute parameter formatting.
#'
#' @param .self The current ADaM domain.
#'
#' @param output_cols Character vector of output column names.
#'
#' @param depend_domains Character vector of domain names that the current
#'   operation depends on.
#'
#' @param depend_columns Character vector of column names that the current
#'   operation depends on.
#'
#' @param action_parameters List or object containing parameters specific to
#'   the action being performed. Used for compute operations when no specific
#'   template match is found.
#'
#' @param node_id Identifier for the current processing node.
#'
#' @param path_trial Character string specifying the file path to the trial
#'   data directory.
#'
#' @param domain_ui_data List containing UI-related domain metadata, including
#'   initialization metadata accessed via `domain_ui_data$init`.
#'
#' @param domain_keys Character vector or list specifying the key columns
#'   that uniquely identify records within the domain.
#'
#' @param is_final_pgm Logical value indicating whether this is the final
#'   program in the processing pipeline. Used for write operations.
#'
#' @param available_data Object or list containing information about currently
#'   available data sources and their status.
#'
#' @return
#' Returns a list or object containing the formatted parameters appropriate
#' for the specified template. The structure and content depend on the
#' `code_id` value:
#' \itemize{
#'   \item For recognized template IDs: Structured parameter list specific to that template
#'   \item For unrecognized IDs: Formatted column compute parameters
#' }
#'
#' @details
#' This function acts as a parameter factory, routing different code generation
#' scenarios to their appropriate parameter formatting functions. Each template
#' type requires different parameter structures and this function ensures the
#' correct parameters are generated for each use case.
#'
#' The function uses a switch statement to dispatch to specialized parameter
#' generation functions based on the template identifier. This design allows
#' for easy extension of new template types while maintaining a consistent
#' interface.
#'
#'
#' @noRd
define_params <- function(
  code_id,
  .self,
  output_cols,
  depend_domains,
  depend_columns,
  action_parameters,
  node_id,
  path_trial,
  domain_ui_data,
  domain_keys,
  is_final_pgm,
  available_data
) {
  init_metadata <- domain_ui_data$init

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
    "_write_data.mustache" = params_write_domain_code(
      .self = .self,
      is_final_pgm = is_final_pgm,
      domain_keys = domain_keys,
      domain_ui_data = domain_ui_data,
      available_data = available_data
    ),
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


gen_node_header <- function(title, no_line_break) {
  n_dash <- 80 - (nchar(title) + 3)
  if (n_dash > 0) {
    title_line <- paste0(" ", paste0(rep("-", n_dash), collapse = ""))
  } else {
    title_line <- "----"
  }

  return(paste0(ifelse(no_line_break, "# ", "\n# "), title, title_line, "\n"))
}

add_hash <- function(text) {
  # Add # at the beginning
  result <- paste0("# ", text)
  # Replace \n with \n#
  result <- gsub("\n", "\n# ", result)
  return(result)
}

#' Format Internal Code ID for Mustache Components
#'
#' @description
#' Transforms internal code IDs (prefixed with "_") into relative paths for
#' mustache components stored in the mighty package, enabling proper file
#' sourcing by get_rendered_component.
#'
#' @param code_id Character string representing the code ID. Internal components
#'   should be prefixed with "_".
#'
#' @return
#' Character string containing the full system path to the component file for
#' internal code IDs, or the original code_id for external components.
#'
#' @noRd
format_internal_code_id <- function(code_id) {
  if (!startsWith(code_id, "_")) {
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
    lapply(collapse_code)
  names(programs) <- program_names
  return(programs)
}

collapse_code <- function(program_i) {
  paste(program_i$code, collapse = "\n")
}
