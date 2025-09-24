#' Checks each actions executable state based on available data
#' @param data_context An R6 object of class \link{data_context} that contains 
#' information on available data in a given \code{connector} object
#'
#' @returns A data.table containing action data with the additional columns
#' @param actions A data.table containing information on dependencies, output columns, actions etc.
#' \code{can_execute}, \code{missing_dependencies}, and \code{lineage} 
#'
add_check_executable_status <-  function(actions, ui_data, data_context) {
  if (is.null(data_context)) {
    return(list(actions = add_default_execution_status(actions), available_columns = NULL))
  }

  available_cols <- determine_available_columns(data_context)

  # initialise actions - will be a copy of actions with modified columns
  # * depend_cols, 
  # * outputs, 
  # added columns 
  # + can_execute, 
  # + missing_dependency, 
  # + removed_outputs, 
  # + lineage
  actions_execute <- data.table()

  for (action_id in seq_len(nrow(actions))) {
    action <- actions[action_id]
    updated_action  <- process_action(action, actions_execute, available_cols, ui_data)    
    actions_execute <- rbind(actions_execute, updated_action)

    available_cols <- update_available_columns_for_action(updated_action, available_cols)
  }

  validate_removed_outputs <-  function(x) {
    (all(sapply(x, is.character)) || all(sapply(x, is.na)))
  }

  validate_removed_depend_cols <- function(x) {
    (length(x) == 1 && is.na(x)) || 
    (is.data.frame(x) && nrow(x) > 0)
  }

  # Apply assertions using the same pattern
  assertthat::assert_that(
    all(sapply(actions_execute$removed_outputs, validate_removed_outputs)),
    msg = "removed_outputs contains invalid data"
  )

  assertthat::assert_that(
    all(sapply(actions_execute$removed_depend_cols, validate_removed_depend_cols)),
    msg = "removed_depend_cols contains invalid data"
  )
  return(list(actions = actions_execute, available_columns = available_cols))
}

#' Process a single action based on its type
process_action <- function(action, processed_actions, available_cols, ui_data = NULL) {
  handler <- switch(action$code_id,
    "_read_data.mustache" = handle_read_data_action,
    "_init_domain.mustache" = handle_init_domain_action,
    "_filter_domain.mustache" = handle_filter_domain_action,
    "_write_data.mustache" = handle_write_domain_action,
    handle_generic_action  # default case
  )

  handler(action, processed_actions, available_cols, ui_data)
}

#' Handler for _read_data.mustache actions
handle_read_data_action <- function(action, processed_actions, available_cols, ui_data) {
  expected_outputs <- parse_output_columns(action$outputs[[1]])
  missing_in_data <- find_missing_columns(expected_outputs, available_cols)
  valid_outputs <- expected_outputs[!column_name %in% missing_in_data$column_name]
  removed_outputs <- find_missing_columns(expected_outputs, valid_outputs)

  valid_outputs <- paste(valid_outputs$domain, valid_outputs$column_name, sep = ".")
  removed_outputs <- paste(removed_outputs$domain, removed_outputs$column_name, sep = ".")
  if (length(removed_outputs) > 0){
    removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  }
  else {
    removed_outputs <- NA
  }
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- NA
  action$removed_outputs <- removed_outputs
  action$can_execute <- (length(valid_outputs) > 0)
  lineage <- ""
  if (action$can_execute && !is.na(removed_outputs)) {
    lineage <- paste("Code can be executed but the following columns are not found in source data:",
      collapse_missing_dependencies(missing_in_data),
      "This impacts the following data that will be ignored:",
      collapse_missing_dependencies(parse_output_columns(removed_outputs[[1]])), sep = "\n")
  }
  else if (!action$can_execute) {
    lineage <- create_lineage_message("Cannot read needed base data:", action, missing_in_data)
  }
  action$lineage <- lineage

  action
}

#' Handler for _init_domain.mustache actions
handle_init_domain_action <- function(action, processed_actions, available_cols, ui_data) {
  # Initialise defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- NA

  # Check if read_data action was lacking outputs
  rda <- processed_actions[program_id == action$program_id & code_id == "_read_data.mustache"][1]
  rda_removed_outputs <- rda$removed_outputs[[1]]
  if (length(rda_removed_outputs) > 1 || !is.na(rda_removed_outputs)) {
    removed_cols <- parse_output_columns(rda_removed_outputs)
    # If outputs from rda were removed, adjust depend_cols
    depend_cols <- dplyr::anti_join(action$depend_cols[[1]], 
      removed_cols, 
      by = c("column_name", "domain", "domain_type"))
    removed_depend_cols <- find_missing_dependencies(
      action$depend_cols[[1]], depend_cols)
    removed_outputs <- unique(removed_cols$column_name)
    valid_outputs <- setdiff(action$outputs[[1]], removed_outputs)
    can_execute = length(valid_outputs) > 0
  }
  action$depend_cols <- list(depend_cols)
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- removed_depend_cols

  # Only remove outputs if they are part of the depend cols
  if (!is.na(removed_depend_cols)) {
    action$removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  }
  else {
    action$removed_outputs <- NA
  }
  action$can_execute <- can_execute
  action$lineage <- create_lineage_message(
    "The following columns are part of the specification but does not exist in source data:", action)

  action
}

#' Handler for _filter_domain.mustache
handle_filter_domain_action <- function(action, processed_actions, available_cols, ui_data) {
  # Filter domain action relies on the `outputs` of read_data action. If 
  # outputs are removed in the read_data action, and filter is depending of 
  # these, filter should not be executable. 
  # `outputs` will be adjusted, since the `render_code` will use this column 
  # for the keep variables, thus ensuring a filter can execute if missing data
  # is not used in the filter.
  #  
  # adjusted to reflect the available data. 
  # Two scenarios must be handled:
  # * domain specific filters
  # * global filters

  # Initialise defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- list()
  # Check if read_data action had missing dependencies
  rda <- processed_actions[program_id == action$program_id & code_id == "_read_data.mustache"][1]
  rda_removed_outputs <- rda$removed_outputs[[1]]

  # Executable status should depend on the filtering condition: 
  # If yaml spec contains a filter on a variable that is removed, filter cannot 
  # execute but else it should be possible
  filtering_columns <- ui_data[[action$domain]]$init$filter_depend_cols
  # Filtering specifications are either depending on the base domains 
  # (listed without dot-separation) or other domains (in which domain 
  # is prefixed with a dot)
  is_base_domain_filter <- !grepl("\\.", filtering_columns)
  filtering_columns_base <- filtering_columns[is_base_domain_filter]
  filtering_columns_other <- filtering_columns[!is_base_domain_filter]
  invalid_filters <- c()

  # Removed outputs in init domain also needs to be considered as removed columns
  ida <- processed_actions[
    program_id == action$program_id & 
    code_id    == "_init_domain.mustache"][1]
  ida_removed <- ida$removed_outputs[[1]]
  if (length(ida_removed) > 1 || !is.na(ida_removed)) {
    ida_removed_outputs <- ida_removed
  }
  else {
    ida_removed_outputs <- list()
  }
  removed_cols <- parse_output_columns(union(rda_removed_outputs, ida_removed_outputs), default_prefix = rda$domain)

  if (!is.null(rda_removed_outputs) || !is.null(ida_removed_outputs))
  {
    removed_outputs <- unique(removed_cols[domain == action$domain]$column_name)
  }
  if (!is.null(rda_removed_outputs) && length(filtering_columns_base)) {
    invalid_filters <-  intersect(filtering_columns, removed_outputs)
  }
  if (!is.null(rda_removed_outputs) && length(filtering_columns_other)) {
    invalid_filters <- c(invalid_filters,  intersect(filtering_columns, rda_removed_outputs))
  }

  can_execute <- length(invalid_filters) == 0
  depend_cols <- dplyr::anti_join(
    action$depend_cols[[1]], 
    removed_cols, 
    by = c("column_name", "domain", "domain_type")
  )

  removed_depend_cols <- find_missing_dependencies(action$depend_cols[[1]], depend_cols)
  valid_outputs <- setdiff(action$outputs[[1]], removed_outputs)

  if (length(removed_outputs) == 0) {
    removed_outputs <- NA
  }
  else {
    removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  }

  action$depend_cols <- list(depend_cols)
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- removed_depend_cols
  action$removed_outputs <- removed_outputs
  action$can_execute <- can_execute
  # Define lineage
  lineage <- ""
  if (can_execute && length(removed_outputs) > 0 && !is.na(removed_outputs)) {
    lineage <- paste("Code can be executed but the following columns are not found in source data and are ignored in the filter step:",
      collapse_missing_dependencies(parse_output_columns(action$removed_outputs[[1]])), sep = "\n")
  }
  else if (!can_execute) {
    lineage <- paste("Filter cannot be applied due to missing column(s):", paste(invalid_filters, collapse = ", "))
  }
  action$lineage <- lineage

  action
  }

#' Handler for _write_domain.mustache action
handle_write_domain_action <- function(action, processed_actions, available_cols, ui_data) {
  # Action may be impacted by previous init_domain action if columns are 
  # removed due to missing data.
  # If this is the case, 
  # * can_execute is set to the init_domain_action$can_execute
  # * if can_execute is true, outputs must be adjusted to remove any
  #   missing columns as identified in init_domain action.

  # Check if this action was impacted by missing data
  # Initialise defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- NA
  # Check if init_domain action was lacking outputs
  ida <- processed_actions[program_id == action$program_id & code_id == "_init_domain.mustache"][1]
  ida_removed_outputs <- ida$removed_outputs[[1]]

  if (!is.null(ida_removed_outputs)) {
    removed_cols <- parse_output_columns(ida_removed_outputs, default_prefix = ida$domain)
    # If outputs from rda were removed, adjust depend_cols
    depend_cols <- dplyr::anti_join(
      action$depend_cols[[1]], 
      removed_cols, 
      by = c("column_name", "domain", "domain_type"))
    removed_depend_cols <- find_missing_dependencies(action$depend_cols[[1]], depend_cols)
    removed_outputs <- unique(removed_cols$column_name)
    valid_outputs <- setdiff(action$outputs[[1]], removed_outputs)
    can_execute <- ida$can_execute
  }

  if (length(removed_outputs) && !is.na(removed_outputs[1])) {
    removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  }
  else {
    removed_outputs <- NA
  }
  action$depend_cols <- list(depend_cols)
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- removed_depend_cols
  action$removed_outputs <- removed_outputs
  action$can_execute <- can_execute
  action$lineage <- create_lineage_message("Cannot write data:", action)

  action
  }

#' Handler for generic actions (default case)
# handle_generic_action <- function(action, next_action, actions, available_cols, action_id) {
handle_generic_action <- function(action, processed_actions, available_cols, ui_data) {
  can_execute <- TRUE
  missing_dependencies <- NA
  removed_outputs <- NA
  valid_outputs <- action$outputs
  depend_cols <- action$depend_cols[[1]]

  # If dependencies are missing, action cannot execute
  if (!(is.null(depend_cols) || length(depend_cols) == 1 && is.na(depend_cols))) {
    missing_dependencies <- find_missing_dependencies(depend_cols, available_cols)
    can_execute <- is.na(missing_dependencies)
  }
  if (!is.na(missing_dependencies)) {
    if (length(valid_outputs) > 0) {
      removed_outputs <- list(prefix_with_domain(valid_outputs[[1]], action$domain))
    }
#    valid_outputs <- NA
  }
  action$outputs <- valid_outputs
  action$removed_depend_cols <- NA
  action$removed_outputs <- removed_outputs
  action$can_execute <- can_execute

  action$lineage <- create_lineage_message(
    "Cannot execute code that is depending on:", action, missing_dependencies[[1]], TRUE)
  action
}

# ============================================================================
# HELPER FUNCTIONS FOR COLUMN OPERATIONS
# ============================================================================

#' Parse output columns from dot-separated format
parse_output_columns <- function(outputs, default_prefix = "") {
  if (is.null(outputs) || length(outputs) == 0 || (length(outputs) == 1 && is.na(outputs))) {
    return(data.table(domain_type = character(), domain = character(), column_name = character()))
  }
  outputs <- prefix_with_domain(outputs, default_prefix)
  split_result <- tstrsplit(outputs, "\\.")
  domain <- split_result[[1]]
  domain_type <- sapply(domain, \(x) classify_data_domains(x))
  data.table(
    domain_type = domain_type,
    domain = domain,
    column_name = split_result[[2]]
  )
}

#' Prefix output columns with `domain` name
prefix_with_domain <- function(column, domain) {
  ifelse (grepl(".", column, fixed = TRUE),
    column,
    paste(domain, column, sep = ".")
  )
}

#' Find columns that are missing from available columns
find_missing_columns <- function(needed_outputs, available_cols) {
  needed_outputs %>% 
    dplyr::anti_join(available_cols, by = c("column_name", "domain"))
}

#' Find missing dependencies by comparing depend_cols with available columns
find_missing_dependencies <-  function(depend_cols, available_cols) {
  if (!is.data.table(depend_cols)) {
    return(NA)
  }
  
  missing_dependencies <- depend_cols %>% 
    dplyr::anti_join(available_cols, by = c("column_name", "domain", "domain_type")) %>%
    # ensure re-ordering of columns such that they can be reported by domain_type, domain, column_name
    dplyr::select(domain_type, domain, column_name)
  if (nrow(missing_dependencies) > 0) {
    return(list(missing_dependencies))
  }
  else {
    return(NA)
  }
}

# ============================================================================
# FUNCTIONS FOR UPDATING AVAILABLE COLUMNS
# ============================================================================

#' Update available columns after action execution
update_available_columns_for_action <- function(action, available_cols) {
  # Handle case where outputs is NA or NULL
  if (action$code_id == "_read_data.mustache" || is.null(action$outputs[[1]]) || (length(action$outputs[[1]]) == 1 && is.na(action$outputs[[1]]))) {
    return(available_cols)
  }
  if (action$can_execute) {
    output_cols <- data.frame(
      column_name = action$outputs[[1]],
      domain = action$domain,
      domain_type = classify_data_domains(action$domain))
    return(unique(rbind(available_cols, output_cols)))
  } 
  return(available_cols)
  
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Create lineage message for missing dependencies
create_lineage_message <- function(message, action, dependencies = NULL, report_missing_outputs = FALSE) {
  if (!action$can_execute) {
    if (is.null(dependencies)) {
      c <- collapse_missing_dependencies(action$removed_depend_cols[[1]])
    }
    else {
      c <- collapse_missing_dependencies(dependencies)
    }
    missing_outputs <- 
      ifelse(
        report_missing_outputs,
        paste(
          "The following columns cannot be created: ", 
          collapse_missing_dependencies(parse_output_columns(action$removed_outputs[[1]], action$domain)),
          sep = "\n"
        ),
        ""
      )
    return(paste(message, c, missing_outputs, sep = "\n"))
  }
  return("")
}

collapse_missing_dependencies <- function(removed_data) {
  paste(apply(removed_data,
        1, paste, collapse = "."), collapse = ", ")
}

#' Add default execution status when no data context is provided
add_default_execution_status <- function(actions) {
  actions_execute <- copy(actions)
  actions_execute$can_execute <- TRUE
  actions_execute$missing_dependency <- list(NA)
  actions_execute$lineage <- ""
  actions_execute$removed_outputs <- list(c(""))
  return(actions_execute)
}

#' Determine available columns from data context
#' @noRd
determine_available_columns <- function(data_context) {
  available_cols <- data.frame(
    column_name = character(),
    domain = character(),
    domain_type = character()
  )

  for (domain_type in data_context$get_datasource_names()) {
    for (domain in data_context$get_tables(domain_type)) {
      for (column_name in names(domain$variables)) {
        available_cols <- rbind(
          available_cols, 
          data.frame(
            column_name = column_name,
            domain = domain$name,
            domain_type = domain_type
          )
        )
      }
    }
  }
  available_cols
}
