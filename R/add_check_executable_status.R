#' Check Executable Status of Actions Based on Available Data
#'
#' @description
#' Evaluates each action's ability to execute by checking data dependencies
#' and updates the actions table with execution status information.
#'
#' @param actions A data.table containing action information including
#'   dependencies, output columns, and action specifications.
#' @param ui_data UI configuration data used during action processing.
#' @param data_context An R6 object of class \link{data_context} containing
#'   information on available data in a given connector object. If NULL,
#'   default execution status is applied.
#'
#' @return
#' A list with two elements:
#' \itemize{
#'   \item `actions` - Enhanced data.table with additional columns:
#'     `can_execute`, `missing_dependencies`, `removed_outputs`,
#'     `removed_depend_cols`, and `lineage`
#'   \item `available_columns` - Updated available columns after processing
#'     all actions, or NULL if data_context is NULL
#' }
#'
#' @details
#' The function processes actions sequentially, updating available data
#' after each action to reflect the cumulative effect on data availability.
#' Actions that cannot execute due to missing dependencies are flagged
#' accordingly.
#'
#' @noRd
add_check_executable_status <- function(actions, ui_data, data_context) {
  if (is.null(data_context)) {
    return(list(
      actions = add_default_execution_status(actions),
      available_columns = NULL
    ))
  }

  available_cols <- determine_available_columns(data_context)

  # initialize actions - will be a copy of actions with modified columns
  # * depend_cols,
  # * outputs,
  # added columns
  # - can_execute,
  # - missing_dependency,
  # - removed_outputs,
  # - lineage
  actions_execute <- data.table()

  for (action_id in seq_len(nrow(actions))) {
    action <- actions[action_id]
    updated_action <- process_action(
      action,
      actions_execute,
      available_cols,
      ui_data
    )
    actions_execute <- rbind(actions_execute, updated_action)

    available_cols <- update_available_columns_for_action(
      updated_action,
      available_cols
    )
  }

  validate_removed_outputs <- function(x) {
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
    all(sapply(
      actions_execute$removed_depend_cols,
      validate_removed_depend_cols
    )),
    msg = "removed_depend_cols contains invalid data"
  )
  return(list(actions = actions_execute, available_columns = available_cols))
}

#' Process a Single Action Based on Its Type
#'
#' @description
#' Routes actions to appropriate handlers based on their code_id, using a
#' switch statement to determine the correct processing function.
#'
#' @details
#' This function serves as a dispatcher that:
#' - Identifies the action type from the code_id field
#' - Routes to specialized handlers for known action types
#' - Falls back to generic handler for unrecognized action types
#' - Maintains consistent interface across all handler functions
#'
#' @param action Action object containing specifications to be processed,
#'   must include a code_id field for handler selection.
#' @param processed_actions Data table of previously processed actions,
#'   passed to handlers for dependency checking.
#' @param available_cols Data frame of currently available columns for
#'   validation and dependency resolution.
#' @param ui_data Optional list containing UI specifications, defaults to
#'   NULL if not provided.
#'
#' @return
#' Modified action object returned from the appropriate handler function
#' with updated execution status, dependencies, outputs, and lineage.
#'
#' @noRd
process_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data = NULL
) {
  handler <- switch(
    action$code_id,
    "mighty_read_data" = handle_read_data_action,
    "mighty_init_domain" = handle_init_domain_action,
    "mighty_filter_domain" = handle_filter_domain_action,
    "mighty_write_data" = handle_write_domain_action,
    handle_generic_action # default case
  )

  handler(action, processed_actions, available_cols, ui_data)
}

#' Handler for mighty_read_data Actions
#'
#' @description
#' Processes read data actions by validating expected outputs against available
#' source data columns and determining which columns can be successfully read.
#'
#' @details
#' This handler manages the initial data reading step by:
#' - Comparing expected output columns with available source data
#' - Identifying missing columns that cannot be read from source
#' - Setting execution status based on whether any valid outputs remain
#' - Creating detailed lineage messages about missing data impacts
#'
#' @param action Action object containing read data specifications with expected
#'   output columns.
#' @param processed_actions Data table of previously processed actions (unused
#'   in this handler).
#' @param available_cols Data frame of columns available in the source data
#'   for validation.
#' @param ui_data List containing UI specifications (unused in this handler).
#'
#' @return
#' Modified action object with updated execution status, valid outputs, removed
#' outputs, and lineage information detailing any missing source data columns.
#'
#' @noRd
handle_read_data_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data
) {
  expected_outputs <- parse_output_columns(action$outputs[[1]])
  missing_in_data <- find_missing_columns(expected_outputs, available_cols)
  valid_outputs <- expected_outputs[
    !column_name %in% missing_in_data$column_name
  ]
  removed_outputs <- find_missing_columns(expected_outputs, valid_outputs)

  valid_outputs <- paste(
    valid_outputs$domain,
    valid_outputs$column_name,
    sep = "."
  )
  removed_outputs <- paste(
    removed_outputs$domain,
    removed_outputs$column_name,
    sep = "."
  )
  if (length(removed_outputs) > 0) {
    removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  } else {
    removed_outputs <- NA
  }
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- NA
  action$removed_outputs <- removed_outputs
  action$can_execute <- (length(valid_outputs) > 0)
  lineage <- ""
  if (action$can_execute && !is.na(removed_outputs)) {
    lineage <- paste(
      "Code can be executed but the following columns are not found in source data:",
      collapse_missing_dependencies(missing_in_data),
      "This impacts the following data that will be ignored:",
      collapse_missing_dependencies(parse_output_columns(removed_outputs[[1]])),
      sep = "\n"
    )
  } else if (!action$can_execute) {
    lineage <- create_lineage_message(
      "Cannot read needed base data:",
      action,
      missing_in_data
    )
  }
  action$lineage <- lineage

  action
}

#' Adjust Action Dependencies Based on Removed Outputs
#'
#' @description
#' Updates an action's dependencies and outputs after columns have been removed
#' from an upstream action.
#'
#' @param action Action object to update.
#' @param removed_cols_output Vector of removed column names (with domain prefix).
#' @param default_prefix Optional domain prefix for parsing column names.
#' @param can_execute_fallback Value to use for can_execute if outputs were removed.
#'   Defaults to checking if valid_outputs has length > 0.
#'
#' @return
#' List with updated values:
#' \itemize{
#'   \item depend_cols - Updated dependency columns
#'   \item removed_depend_cols - Missing dependencies
#'   \item removed_outputs - Removed output column names
#'   \item valid_outputs - Remaining valid outputs
#'   \item can_execute - Whether action can still execute
#' }
#'
#' @noRd
derive_updated_action_values <- function(
  action,
  removed_cols_output,
  default_prefix = NULL,
  can_execute_fallback = NULL
) {
  # Parse removed columns
  removed_cols <- if (is.null(default_prefix)) {
    parse_output_columns(removed_cols_output)
  } else {
    parse_output_columns(removed_cols_output, default_prefix = default_prefix)
  }

  # Adjust depend_cols by removing the removed columns
  depend_cols <- dplyr::anti_join(
    action$depend_cols[[1]],
    removed_cols,
    by = c("column_name", "domain", "domain_type")
  )

  # Find missing dependencies
  removed_depend_cols <- find_missing_dependencies(
    action$depend_cols[[1]],
    depend_cols
  )

  # Calculate valid outputs
  removed_outputs <- unique(removed_cols$column_name)
  valid_outputs <- setdiff(action$outputs[[1]], removed_outputs)

  # Determine if can execute
  can_execute <- if (!is.null(can_execute_fallback)) {
    can_execute_fallback
  } else {
    length(valid_outputs) > 0
  }

  list(
    depend_cols = depend_cols,
    removed_depend_cols = removed_depend_cols,
    removed_outputs = removed_outputs,
    valid_outputs = valid_outputs,
    can_execute = can_execute
  )
}

#' Handler for mighty_init_domain Actions
#'
#' @description
#' Processes init domain actions by checking dependencies against read_data
#' outputs and adjusting available columns based on missing source data.
#'
#' @details
#' This handler specifically manages the initialization of domain processing by:
#' - Checking if the corresponding read_data action had missing outputs
#' - Adjusting dependencies and outputs based on available source columns
#' - Determining if the action can execute with remaining valid outputs
#'
#' @param action Action object containing init domain specifications.
#' @param processed_actions Data table of previously processed actions, used
#'   to find the corresponding read_data action.
#' @param available_cols Data frame of currently available columns (unused
#'   in this handler).
#' @param ui_data List containing UI specifications (unused in this handler).
#'
#' @return
#' Modified action object with updated execution status, dependencies, outputs,
#' and lineage information reflecting any columns missing from source data.
#'
#' @noRd
handle_init_domain_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data
) {
  # Initialize defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- NA

  # Check if read_data action was lacking outputs
  rda <- processed_actions[
    program_id == action$program_id & code_id == "mighty_read_data"
  ][1]
  rda_removed_outputs <- rda$removed_outputs[[1]]
  if (length(rda_removed_outputs) > 1 || !is.na(rda_removed_outputs)) {
    adjusted <- derive_updated_action_values(
      action,
      rda_removed_outputs
    )
    depend_cols <- adjusted$depend_cols
    removed_depend_cols <- adjusted$removed_depend_cols
    removed_outputs <- adjusted$removed_outputs
    valid_outputs <- adjusted$valid_outputs
    can_execute <- adjusted$can_execute
  }
  action$depend_cols <- list(depend_cols)
  action$outputs <- list(valid_outputs)
  action$removed_depend_cols <- removed_depend_cols

  # Only remove outputs if they are part of the depend cols
  if (!is.na(removed_depend_cols)) {
    action$removed_outputs <- list(prefix_with_domain(
      removed_outputs,
      action$domain
    ))
  } else {
    action$removed_outputs <- NA
  }
  action$can_execute <- can_execute
  action$lineage <- create_lineage_message(
    "The following columns are part of the specification but does not exist in source data:",
    action
  )

  action
}

#' Handler for mighty_filter_domain
#'
#' @description
#' Processes filter domain actions by checking dependencies against available
#' columns and handling cases where filtering columns are missing from source data.
#'
#' @details
#' This handler manages two scenarios:
#' - Domain-specific filters (columns without dot separation)
#' - Global filters (columns with domain prefixes)
#'
#' The function determines if a filter can execute based on whether the required
#' filtering columns are available in the processed data.
#'
#' @param action Action object containing filter domain specifications.
#' @param processed_actions Data table of previously processed actions.
#' @param available_cols Data frame of currently available columns.
#' @param ui_data List containing UI specifications including filter dependencies.
#'
#' @return
#' Modified action object with updated execution status, dependencies, outputs,
#' and lineage information.
#'
#' @noRd
handle_filter_domain_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data
) {
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

  # Initialize defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- list()
  # Check if read_data action had missing dependencies
  rda <- processed_actions[
    program_id == action$program_id & code_id == "mighty_read_data"
  ][1]
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
      code_id == "mighty_init_domain"
  ][1]
  ida_removed <- ida$removed_outputs[[1]]
  if (length(ida_removed) > 1 || !is.na(ida_removed)) {
    ida_removed_outputs <- ida_removed
  } else {
    ida_removed_outputs <- list()
  }
  removed_cols <- parse_output_columns(
    union(rda_removed_outputs, ida_removed_outputs),
    default_prefix = rda$domain
  )

  if (!is.null(rda_removed_outputs) || !is.null(ida_removed_outputs)) {
    removed_outputs <- unique(removed_cols[domain == action$domain]$column_name)
  }
  if (!is.null(rda_removed_outputs) && length(filtering_columns_base)) {
    invalid_filters <- intersect(filtering_columns, removed_outputs)
  }
  if (!is.null(rda_removed_outputs) && length(filtering_columns_other)) {
    invalid_filters <- c(
      invalid_filters,
      intersect(filtering_columns, rda_removed_outputs)
    )
  }

  can_execute <- length(invalid_filters) == 0
  depend_cols <- dplyr::anti_join(
    action$depend_cols[[1]],
    removed_cols,
    by = c("column_name", "domain", "domain_type")
  )

  removed_depend_cols <- find_missing_dependencies(
    action$depend_cols[[1]],
    depend_cols
  )
  valid_outputs <- setdiff(action$outputs[[1]], removed_outputs)

  if (length(removed_outputs) == 0) {
    removed_outputs <- NA
  } else {
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
    lineage <- paste(
      "Code can be executed but the following columns are not found in source data and are ignored in the filter step:",
      collapse_missing_dependencies(parse_output_columns(action$removed_outputs[[
        1
      ]])),
      sep = "\n"
    )
  } else if (!can_execute) {
    lineage <- paste(
      "Filter cannot be applied due to missing column(s):",
      paste(invalid_filters, collapse = ", ")
    )
  }
  action$lineage <- lineage

  action
}

#' Handler for _write_domain.mustache Action
#'
#' @description
#' Processes write domain actions by checking dependencies against the
#' corresponding init_domain action and adjusting outputs based on any
#' columns that were removed due to missing source data.
#'
#' @details
#' This handler manages the final data writing step by:
#' - Checking if the corresponding init_domain action had missing outputs
#' - Inheriting execution status from the init_domain action
#' - Adjusting dependencies and outputs to exclude any unavailable columns
#' - Ensuring consistency between initialization and writing phases
#'
#' @param action Action object containing write domain specifications.
#' @param processed_actions Data table of previously processed actions, used
#'   to find the corresponding init_domain action.
#' @param available_cols Data frame of currently available columns (unused
#'   in this handler).
#' @param ui_data List containing UI specifications (unused in this handler).
#'
#' @return
#' Modified action object with updated execution status, dependencies, outputs,
#' and lineage information reflecting any columns that cannot be written due
#' to missing source data.
#'
#' @noRd
handle_write_domain_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data
) {
  # Action may be impacted by previous init_domain action if columns are
  # removed due to missing data.
  # If this is the case,
  # * can_execute is set to the init_domain_action$can_execute
  # * if can_execute is true, outputs must be adjusted to remove any
  #   missing columns as identified in init_domain action.

  # Check if this action was impacted by missing data
  # Initialize defaults
  can_execute <- TRUE
  valid_outputs <- action$outputs[[1]]
  removed_outputs <- NA
  depend_cols <- action$depend_cols[[1]]
  removed_depend_cols <- NA
  # Check if init_domain action was lacking outputs
  ida <- processed_actions[
    program_id == action$program_id & code_id == "mighty_init_domain"
  ][1]
  ida_removed_outputs <- ida$removed_outputs[[1]]

  if (!is.null(ida_removed_outputs)) {
    adjusted <- derive_updated_action_values(
      action,
      ida_removed_outputs,
      default_prefix = ida$domain,
      can_execute_fallback = ida$can_execute
    )
    depend_cols <- adjusted$depend_cols
    removed_depend_cols <- adjusted$removed_depend_cols
    removed_outputs <- adjusted$removed_outputs
    valid_outputs <- adjusted$valid_outputs
    can_execute <- adjusted$can_execute
  }

  if (length(removed_outputs) && !is.na(removed_outputs[1])) {
    removed_outputs <- list(prefix_with_domain(removed_outputs, action$domain))
  } else {
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

#' Handler for Generic Actions (Default Case)
#'
#' @description
#' Processes generic actions by checking if all required dependencies are
#' available and determining execution status accordingly.
#'
#' @details
#' This is the default handler for actions that don't require special processing.
#' It validates dependencies against available columns and sets execution status
#' based on whether all required columns are present.
#'
#' @param action Action object containing generic action specifications.
#' @param processed_actions Data table of previously processed actions (unused
#'   in generic handler).
#' @param available_cols Data frame of currently available columns for dependency
#'   checking.
#' @param ui_data List containing UI specifications (unused in generic handler).
#'
#' @return
#' Modified action object with updated execution status, dependencies, outputs,
#' and lineage information indicating any missing dependencies.
#'
#' @noRd
handle_generic_action <- function(
  action,
  processed_actions,
  available_cols,
  ui_data
) {
  can_execute <- TRUE
  missing_dependencies <- NA
  removed_outputs <- NA
  valid_outputs <- action$outputs
  depend_cols <- action$depend_cols[[1]]

  # If dependencies are missing, action cannot execute
  if (
    !(is.null(depend_cols) || length(depend_cols) == 1 && is.na(depend_cols))
  ) {
    missing_dependencies <- find_missing_dependencies(
      depend_cols,
      available_cols
    )
    can_execute <- is.na(missing_dependencies)
  }
  if (!is.na(missing_dependencies)) {
    if (length(valid_outputs) > 0) {
      removed_outputs <- list(prefix_with_domain(
        valid_outputs[[1]],
        action$domain
      ))
    }
  }
  action$outputs <- valid_outputs
  action$removed_depend_cols <- NA
  action$removed_outputs <- removed_outputs
  action$can_execute <- can_execute

  action$lineage <- create_lineage_message(
    "Cannot execute code that is depending on:",
    action,
    missing_dependencies[[1]],
    TRUE
  )
  action
}

# ============================================================================
# HELPER FUNCTIONS FOR COLUMN OPERATIONS
# ============================================================================

#' Parse Output Columns from Dot-Separated Format
#'
#' @description
#' Converts dot-separated column specifications into a structured data table
#' with domain, domain type, and column name information.
#'
#' @details
#' Handles column specifications in the format "domain.column_name" by splitting
#' on the dot separator and classifying domains. If no domain prefix is present,
#' uses the default_prefix parameter.
#'
#' @param outputs Character vector of column specifications, potentially in
#'   dot-separated format (e.g., "ADSL.USUBJID").
#' @param default_prefix Character string to use as domain prefix when columns
#'   don't contain explicit domain specification.
#'
#' @return
#' Data table with columns: domain_type, domain, and column_name. Returns
#' empty data table if outputs is NULL, empty, or contains only NA values.
#' @noRd
parse_output_columns <- function(outputs, default_prefix = "") {
  if (
    is.null(outputs) ||
      length(outputs) == 0 ||
      (length(outputs) == 1 && is.na(outputs))
  ) {
    return(data.table(
      domain_type = character(),
      domain = character(),
      column_name = character()
    ))
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

#' Prefix Output Columns with Domain Name
#'
#' @description
#' Adds domain prefix to column names that don't already contain a dot separator,
#' creating standardized dot-separated column specifications.
#'
#' @details
#' Checks each column name for the presence of a dot separator. If no dot is
#' found, prefixes the column with the specified domain name. Columns already
#' containing dots are returned unchanged.
#'
#' @param column Character vector of column names to potentially prefix.
#' @param domain Character string specifying the domain name to use as prefix.
#'
#' @return
#' Character vector with domain-prefixed column names in the format
#' "domain.column_name" for columns that didn't already contain dots.
#' @noRd
prefix_with_domain <- function(column, domain) {
  ifelse(
    grepl(".", column, fixed = TRUE),
    column,
    paste(domain, column, sep = ".")
  )
}

#' Find Columns That Are Missing from Available Columns
#'
#' @description
#' Identifies columns that are needed but not present in the available columns
#' by comparing domain and column name combinations.
#'
#' @param needed_outputs Data frame with columns "column_name" and "domain"
#'   representing required columns.
#' @param available_cols Data frame with columns "column_name" and "domain"
#'   representing available columns.
#'
#' @return
#' Data frame containing the missing columns with "column_name" and "domain" columns.
#' @noRd
find_missing_columns <- function(needed_outputs, available_cols) {
  needed_outputs |>
    dplyr::anti_join(available_cols, by = c("column_name", "domain"))
}

#' Find Missing Dependencies
#'
#' @description
#' Identifies dependency columns that are required but not available by comparing
#' domain type, domain, and column name combinations.
#'
#' @param depend_cols Data frame with columns "column_name", "domain", and
#'   "domain_type" representing required dependency columns.
#' @param available_cols Data frame with columns "column_name", "domain", and
#'   "domain_type" representing available columns.
#'
#' @return
#' List containing a data frame of missing dependencies with columns ordered as
#' "domain_type", "domain", "column_name", or NA if no dependencies are missing
#' or input is invalid.
#'
#' @noRd
find_missing_dependencies <- function(depend_cols, available_cols) {
  if (!is.data.table(depend_cols)) {
    return(NA)
  }

  missing_dependencies <- depend_cols |>
    dplyr::anti_join(
      available_cols,
      by = c("column_name", "domain", "domain_type")
    ) |>
    # ensure re-ordering of columns such that they can be reported by domain_type, domain, column_name
    dplyr::select(domain_type, domain, column_name)
  if (nrow(missing_dependencies) > 0) {
    return(list(missing_dependencies))
  } else {
    return(NA)
  }
}

# ============================================================================
# FUNCTIONS FOR UPDATING AVAILABLE COLUMNS
# ============================================================================

#' Update Available Columns After Action Execution
#'
#' @description
#' Updates the available columns list by adding output columns from an executed
#' action, with special handling for data reading operations.
#'
#' @param action Action object containing execution status, outputs, domain, and code_id.
#' @param available_cols Data frame with columns "column_name", "domain", and
#'   "domain_type" representing currently available columns.
#'
#' @return
#' Updated data frame of available columns including new outputs if the action
#' can execute, otherwise returns the original available_cols unchanged.
#' @noRd
update_available_columns_for_action <- function(action, available_cols) {
  # Handle case where outputs is NA or NULL
  if (
    action$code_id == "mighty_read_data" ||
      is.null(action$outputs[[1]]) ||
      (length(action$outputs[[1]]) == 1 && is.na(action$outputs[[1]]))
  ) {
    return(available_cols)
  }
  if (action$can_execute) {
    output_cols <- data.frame(
      column_name = action$outputs[[1]],
      domain = action$domain,
      domain_type = classify_data_domains(action$domain)
    )
    return(unique(rbind(available_cols, output_cols)))
  }
  return(available_cols)
}

# ============================================================================
# UTILITY FUNCTIONS
# ============================================================================

#' Create Lineage Message for Missing Dependencies
#'
#' @description
#' Generates a formatted error message for actions that cannot execute due to
#' missing dependencies.
#'
#' @param message Base error message to display.
#' @param action Action object containing execution status and dependency information.
#' @param dependencies Optional vector of missing dependencies to override action's dependencies.
#' @param report_missing_outputs Logical, whether to include missing output columns
#'   in the message (default FALSE).
#'
#' @return
#' Character string containing the formatted lineage message, or empty string
#' if action can execute.
#'
#' @noRd
create_lineage_message <- function(
  message,
  action,
  dependencies = NULL,
  report_missing_outputs = FALSE
) {
  if (!action$can_execute) {
    if (is.null(dependencies)) {
      c <- collapse_missing_dependencies(action$removed_depend_cols[[1]])
    } else {
      c <- collapse_missing_dependencies(dependencies)
    }
    missing_outputs <-
      ifelse(
        report_missing_outputs,
        paste(
          "The following columns cannot be created: ",
          collapse_missing_dependencies(parse_output_columns(
            action$removed_outputs[[1]],
            action$domain
          )),
          sep = "\n"
        ),
        ""
      )
    return(paste(message, c, missing_outputs, sep = "\n"))
  }
  return("")
}

collapse_missing_dependencies <- function(removed_data) {
  paste(apply(removed_data, 1, paste, collapse = "."), collapse = ", ")
}

#' Add Default Execution Status to Actions
#'
#' @description
#' Sets default execution status for all actions when no data context is available.
#'
#' @param actions A data.table containing action information.
#'
#' @return
#' A copy of the input data.table with added columns describing execution status.
#' `missing_dependency` (NA), `lineage` (""), and `removed_outputs` ("").
#' @noRd
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
