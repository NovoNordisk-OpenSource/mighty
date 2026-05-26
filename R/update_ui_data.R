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
  # Validate inputs
  checkmate::assert_list(code_component_metadata)
  checkmate::assert_data_frame(ui_data, min.rows = 1)
  checkmate::assert_names(
    names(ui_data),
    must.include = c("code_id", "depend_cols", "outputs", "domain")
  )

  if (length(code_component_metadata) > 0) {
    # Extract and prepare metadata for active code IDs
    code_id_data <- extract_code_component_metadata(code_component_metadata)

    # Merge UI data with code metadata
    ui_data_updated <- merge_ui_with_component_metadata(ui_data, code_id_data)
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

  # Determine col_rename vs col_mutate based on whether source is also a col_copy
  col_copy_rows <- ui_data_updated[ui_data_updated$type == "col_copy"]
  if (nrow(col_copy_rows) > 0) {
    col_copy_by_domain <- col_copy_rows[,
      .(col = unlist(outputs)),
      by = domain
    ]
  } else {
    col_copy_by_domain <- data.table::data.table(
      domain = character(0),
      col = character(0)
    )
  }
  all_outputs_by_domain <- ui_data_updated[,
    .(col = unlist(outputs)),
    by = domain
  ]
  rename_rows <- which(ui_data_updated$type == "col_rename")
  for (i in rename_rows) {
    source_col <- unlist(ui_data_updated$depend_cols[[i]])
    domain_i <- ui_data_updated$domain[[i]]
    source_is_col_copy <- source_col %in%
      col_copy_by_domain[domain == domain_i]$col
    # Also downgrade if the source column is itself a declared output — a
    # destructive rename would remove it, but it is still needed downstream
    source_is_also_output <- source_col %in%
      all_outputs_by_domain[domain == domain_i]$col
    if (source_is_col_copy || source_is_also_output) {
      data.table::set(ui_data_updated, i, "type", "col_mutate")
      data.table::set(ui_data_updated, i, "code_id", "_col_mutate.mustache")
    } else {
      data.table::set(ui_data_updated, i, "code_id", "_col_rename.mustache")
    }
  }

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
      column = "col_compute",
      parameter = ,
      row = "row_compute",
      cli::cli_abort(c(
        "Invalid action type {.val {type_from_code}} for code component {.val {code_id}}.",
        "i" = paste0("Domain: ", format_domain(domain)),
        "i" = paste0(
          cli::format_inline("{cli::qty(length(outputs))}Output column{?s}: "),
          format_list(unlist(outputs), format_column)
        ),
        "i" = "Expected type to be one of: {.val {c('column', 'row', 'parameter', 'internal')}}"
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

  # Depends on a different local column -> rename (or mutate if conflict)
  if (is_local && is_different_column) {
    return("col_rename")
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
    "i" = paste0("Domain: ", format_domain(domain)),
    "i" = paste0(
      cli::format_inline("{cli::qty(length(outputs))}Output column{?s}: "),
      format_list(unlist(outputs), format_column)
    ),
    "i" = "Dependencies: {.val {depend_cols}}"
  ))
}

#' Extract and format metadata for active code IDs
#' @description
#' Converts code component metadata from a named list to a data table with one row per component.
#' Preserves mixed-type list-columns (e.g., depend_cols can be data.frame or list).
#'
#' @param code_component_metadata List of metadata for code components, indexed by code_id
#'
#' @return Data table with formatted metadata for active code IDs
#' @noRd
extract_code_component_metadata <- function(code_component_metadata) {
  metadata_transposed <- purrr::list_transpose(
    code_component_metadata,
    simplify = FALSE
  )
  metadata_transposed$type <- unlist(metadata_transposed$type, FALSE)

  data.table::data.table(
    node_id = names(code_component_metadata),
    type = metadata_transposed$type,
    depend_cols = metadata_transposed$depend_cols,
    outputs = metadata_transposed$outputs,
    parameters_defaults = metadata_transposed$parameters_defaults
  )
}

#' Merge UI data with code metadata
#'
#' @description
#' Enriches UI data with metadata from code components, where code component
#' metadata takes precedence over UI-specified values for `depend_cols` and `outputs`.
#'
#' @param ui_data Data table with columns including `node_id`, `code_id`,
#'   `depend_cols`, and `outputs`
#' @param code_id_data Data table from `extract_code_component_metadata()`,
#'   or NULL if no code components exist
#'
#' @return The input `ui_data` with `depend_cols` and `outputs` replaced by
#'   code component values where `code_id` is present, plus a `type_from_code` column
#' @noRd
merge_ui_with_component_metadata <- function(ui_data, code_id_data) {
  if (is.null(code_id_data)) {
    return(ui_data)
  }

  # Avoid modifying caller's data
  code_metadata <- data.table::copy(code_id_data)
  data.table::setnames(
    code_metadata,
    setdiff(names(code_metadata), "node_id"),
    \(x) paste0(x, "_from_code")
  )

  merged_data <- merge(
    ui_data,
    code_metadata,
    by = "node_id",
    all.x = TRUE
  )

  assert_consistent_component_params(merged_data)
  assert_code_outputs_in_yaml(merged_data)

  # Code metadata is source of truth. Column dependencies specified in YAML is prohibited per mighty.json schema
  merged_data[
    !is.na(code_id),
    `:=`(
      depend_cols = depend_cols_from_code,
      outputs = outputs_from_code
    )
  ]
  merged_data[, c("depend_cols_from_code", "outputs_from_code") := NULL]

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
#'     Comes from YAML `method` field, e.g. `"ADSL.TRTSDT"` or `"TRTSDT"`.
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

  # Only col_echo, col_rename, and col_mutate reach here - their depend_cols
  # comes from the YAML method field, which is always a single value
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
#' Generates and adds unique node IDs by combining domain with either an explicit id (if present)
#' or formatted outputs. Node IDs follow the pattern "domain-id" or "domain-output1-output2-...".
#'
#' @param nodes Data table to add node IDs to
#'
#' @return Data table with node_id column added
#' @noRd
add_node_id <- function(nodes) {
  formatted_outputs <- format_outputs(nodes$outputs)

  # Use explicit id if present, otherwise fall back to formatted outputs
  has_id <- !is.na(nodes$id)
  nodes[,
    node_id := data.table::fifelse(
      has_id,
      paste0(domain, "-", id),
      paste0(domain, "-", formatted_outputs)
    )
  ]

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
