#' Update UI data with code component metadata
#' @description
#' Merges UI data with code component metadata based on code_id references and processes dependent columns.
#' @details
#' Takes UI data that may contain code_id references and enriches it with metadata
#' from the code component metadata. Handles the merging of metadata, standardization of terminology,
#' and processing of dependent columns.
#'
#' @param code_component_metadata List of metadata for code components, indexed by code_id
#' @param ui_data Data table containing UI data with optional code_id references
#'
#' @return A data table with UI data enriched with code component metadata and processed dependent columns
#' @noRd
#'
#' @examples
#' # Example usage would go here
consolidate_metadata <- function(code_component_metadata, ui_data) {
  if (length(code_component_metadata) > 0) {
    # Extract and prepare metadata for active code IDs
    code_id_data <- extract_code_component_metadata(code_component_metadata)

    # Merge UI data with code metadata
    ui_data_updated <- merge_ui_with_metadata(ui_data, code_id_data)
  } else {
    ui_data_updated <- ui_data[, "type_from_code" := NA_character_]
  }

  # Assign action types to predecessor action types
  ui_data_updated[["type"]] <- purrr::pmap(
    ui_data_updated[, list(
      code_id = get("code_id"),
      type_from_code = get("type_from_code"),
      depend_cols = get("depend_cols"),
      outputs = get("outputs"),
      domain = get("domain")
    )],
    classify_action_type
  ) |>
    unlist()
  ui_data_updated[, "type_from_code" := NULL]

  # Assign internal code_id for predecessor action types
  ui_data_updated[
    ui_data_updated$type == "col_echo",
    "code_id" := "_col_echo.mustache"
  ]
  ui_data_updated[
    ui_data_updated$type == "col_mutate",
    "code_id" := "_col_mutate.mustache"
  ]

  # Process dependent columns
  processed_data <- process_depend_cols(ui_data_updated)

  return(processed_data)
}

classify_action_type <- function(
  code_id,
  type_from_code,
  depend_cols,
  outputs,
  domain
) {
  # Type determined by code component metadata
  if (!is.na(code_id)) {
    return(switch(
      type_from_code,
      derivation = ,
      predecessor = "col_compute",
      row = "row_compute",
      cli::cli_abort(c(
        "Invalid action type {.val {type_from_code}} for code component {.val {code_id}}.",
        "i" = "Domain: {.val {domain}}",
        "i" = "Output column{?s}: {.val {outputs}}",
        "i" = "Expected type to be one of: {.val {c('derivation', 'predecessor', 'row')}}"
      ))
    ))
  }

  # No dependencies -> simple copy from source
  if (purrr::every(depend_cols, is.na)) {
    return("col_copy")
  }

  dependency <- unlist(depend_cols)
  checkmate::assert_character(dependency, len = 1L, any.missing = FALSE)
  is_local <- !has_domain_prefix(dependency)
  is_different_column <- dependency != unlist(outputs)

  # Depends on a different local column -> mutation
  if (is_local && is_different_column) {
    return("col_mutate")
  }

  # Depends on column from another domain -> echo
  if (has_domain_prefix(dependency)) {
    dep_domain <- extract_domain_prefix(dependency)
    if (domain != dep_domain) {
      return("col_echo")
    }
  }

  cli::cli_abort(c(
    "Unable to determine action type.",
    "i" = "Domain: {.val {domain}}",
    "i" = "Output column{?s}: {.val {outputs}}",
    "i" = "Dependencies: {.val {depend_cols}}"
  ))
}

#' Extract and format metadata for active code IDs
#' @description
#' Retrieves and formats metadata for code IDs referenced in the UI data.
#' @details
#' Extracts metadata for active code IDs from the code component metadata,
#' transposes the list structure, and formats it as a data table. Also standardizes
#' terminology by converting "derivation" type to "compute".
#'
#' @param code_component_metadata List of metadata for code components, indexed by code_id
#'
#' @return Data table with formatted metadata for active code IDs, or NULL if no metadata found
#' @noRd
extract_code_component_metadata <- function(code_component_metadata) {
  # Skip if no metadata found
  if (length(code_component_metadata) == 0) {
    return(NULL)
  }

  metadata_transposed <- purrr::list_transpose(
    code_component_metadata,
    simplify = FALSE
  )
  metadata_transposed$type <- metadata_transposed$type |> unlist(FALSE)

  code_id_data <- data.table::data.table(
    node_id = names(code_component_metadata),
    type = metadata_transposed$type,
    depend_cols = metadata_transposed$depend_cols,
    outputs = metadata_transposed$outputs,
    parameters_defaults = metadata_transposed$parameters_defaults
  )
  return(code_id_data)
}

#' Merge UI data with code metadata
#' @description
#' Combines UI data with code metadata based on code_id and consolidates redundant columns.
#' @details
#' Merges UI data with code metadata, validates that outputs are identical,
#' consolidates redundant columns, and cleans up temporary columns created during the merge.
#'
#' @param ui_data Original UI data with code_id references
#' @param code_id_data Metadata for code IDs, formatted as a data table
#'
#' @return Merged data table with consolidated columns
#' @noRd
merge_ui_with_metadata <- function(ui_data, code_id_data) {
  if (is.null(code_id_data)) {
    return(ui_data)
  }

  rnm_cols <- names(code_id_data) != "node_id"
  names(code_id_data)[rnm_cols] <- paste0(
    names(code_id_data)[rnm_cols],
    "_from_code"
  )

  merged_data <- merge(
    ui_data,
    code_id_data,
    by = "node_id",
    all.x = TRUE
  )

  # Validate outputs consistency
  assert_outputs_identical(merged_data)

  # Consolidate columns for data that is redundant
  merged_data[
    !is.na(code_id),
    `:=`(
      depend_cols = depend_cols_from_code,
      outputs = outputs_from_code
    )
  ]

  # Clean up redundant columns
  merged_data$outputs_from_code <- NULL
  merged_data$depend_cols_from_code <- NULL

  return(merged_data)
}

#' Process dependent columns in UI data
#' @description
#' Processes the depend_cols column in UI data by applying domain-specific transformations.
#' @details
#' Applies the process_action_depend_cols function to each action's depend_cols
#' and domain values, transforming the depend_cols into a structured format.
#'
#' @param data UI data with depend_cols to process
#'
#' @return Data with processed depend_cols column
#' @noRd
process_depend_cols <- function(data) {
  results <- purrr::pmap(
    data[, .(type, depend_cols, domain, outputs)],
    process_action_depend_cols
  )
  data[, depend_cols := results]
  return(data)
}

#' Process Column Dependencies for a Single Action
#'
#' @description
#' Transforms column dependencies into a structured data table with domain information.
#'
#' @param type Character string specifying the action type (e.g., "col_copy", "col_compute").
#' @param depend_cols Column dependencies. Format depends on action type:
#'   - **data.frame** (with `domain`, `column` cols): For actions with a `code_id`
#'     (col_compute, row_compute). Parsed from code component roxygen `@depends` tags.
#'   - **list of strings**: For actions without a `code_id` (col_echo, col_mutate).
#'     Comes from YAML `method` field, e.g. `"adsl.TRTSDT"` or `"TRTSDT"`.
#'   - **NA**: For col_copy actions (dependencies are inferred from outputs).
#' @param domain Character string specifying the current domain value to use for
#'   elements without domain prefixes.
#' @param outputs List of outputs from the action, used for col_copy self-dependencies.
#'
#' @return
#' Data table with processed dependencies containing columns: column_name, domain,
#' and domain_type. Returns empty data table if no valid dependencies are found.
#'
#' @noRd
process_action_depend_cols <- function(type, depend_cols, domain, outputs) {
  checkmate::assert_string(type)
  checkmate::assert_string(domain)
  checkmate::assert(
    checkmate::check_data_frame(depend_cols),
    checkmate::check_list(depend_cols),
    checkmate::check_atomic(depend_cols)
  )

  # col_copy actions have dependencies on themselves
  if (type == "col_copy") {
    return(data.table::data.table(
      column_name = outputs,
      domain = domain,
      domain_type = classify_data_domains(domain)
    ))
  }

  # All col_compute and row_compute come via mighty.component which
  # provides the depend_cols as a data.frame
  if (is.data.frame(depend_cols)) {
    if (nrow(depend_cols) == 0) {
      return(empty_dependency_table())
    }
    return(data.table::data.table(
      column_name = depend_cols$column,
      domain = depend_cols$domain,
      domain_type = classify_data_domains(depend_cols$domain)
    ))
  }

  # Only col_echo and col_mutate reach here - their depend_cols comes from
  # the YAML method field, which is always a single value
  elements <- unlist(depend_cols)
  checkmate::assert_character(elements, len = 1L, any.missing = FALSE)

  parsed_domain <- if (has_domain_prefix(elements)) {
    extract_domain_prefix(elements)
  } else {
    domain
  }
  parsed_id <- extract_dependency_id(elements)

  data.table::data.table(
    column_name = parsed_id,
    domain = parsed_domain,
    domain_type = classify_data_domains(parsed_domain)
  )
}

#' Create an empty dependency table
#' @noRd
empty_dependency_table <- function() {
  data.table::data.table(
    column_name = character(0),
    domain = character(0),
    domain_type = character(0)
  )
}


#' Add node IDs to data
#' @description
#' Generates and adds unique node IDs to a data table based on domain, outputs, code_id, and parameters.
#' @details
#' Creates unique node IDs by combining domain information with formatted
#' outputs, code_id, and parameters. Optimized for performance with vectorized operations.
#'
#' @param nodes Data table to add node IDs to
#'
#' @return Data table with node_id column added
#' @noRd
add_node_id <- function(nodes) {
  # Format components of the node ID
  formatted_outputs <- format_outputs(nodes$outputs)

  # Combine components to create node ID
  nodes$node_id <- lapply(seq_len(nrow(nodes)), function(i) {
    domain <- nodes$domain[[i]]
    if (!is.na(nodes$id[[i]])) {
      paste0(domain, "-", nodes$id[[i]])
    } else {
      paste0(
        domain,
        "-",
        formatted_outputs[[i]]
      )
    }
  }) |>
    unlist()

  return(nodes)
}

#' Format outputs for node ID
#' @description
#' Formats the outputs list for inclusion in node IDs.
#' @details
#' Converts each output list to a hyphen-separated string, or returns an empty string for NA values.
#'
#' @param outputs List of outputs to format
#'
#' @return Vector of formatted output strings
#' @noRd
format_outputs <- function(outputs) {
  vapply(
    outputs,
    function(x) {
      if (anyNA(x)) "" else paste0(unlist(x), collapse = "-")
    },
    character(1)
  )
}
