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
#' @export
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
    pred_type
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

pred_type <- function(code_id, type_from_code, depend_cols, outputs, domain) {
  # Check if code_id is provided and retrieve the type
  if (!is.na(code_id)) {
    if (type_from_code %in% c("derivation", "predecessor")) {
      return("col_compute")
    } else if (type_from_code == "row") {
      return("row_compute")
    } else {
      stop(
        paste0("Invalid action type ", type_from_code, " for ", code_id),
        "."
      )
    }
  }

  # Check if depend_cols are all NA
  if (all(sapply(depend_cols, is.na))) {
    return("col_copy")
  }

  if (!grepl("\\.", depend_cols[[1]]) && depend_cols[[1]] != outputs[[1]]) {
    return("col_mutate")
  }

  # Split the depend_cols and take the first part directly
  v <- strsplit(depend_cols[!is.na(depend_cols)], "\\.")

  # Check if there's a valid dependency
  if (length(v[[1]]) == 2) {
    dep_domain <- v[[1]][1] # Get the first part of the first valid depend_col
    if (domain != dep_domain) {
      return("col_echo")
    }
  }

  # If no criteria are met
  stop("Unable to determine action type.")
}


#' Check if UI data contains code_id references
#' @description
#' Determines whether the UI data contains any non-NA code_id references.
#' @details
#' Checks if there are any active code_id references in the UI data
#' that would require fetching and merging metadata.
#'
#' @param ui_data Data table to check for code_id references
#'
#' @return Logical value: TRUE if code_id references exist, FALSE otherwise
has_code_id_references <- function(ui_data) {
  active_code_ids <- unique(ui_data[
    !is.na(code_id) & !grepl("^_", code_id),
    code_id
  ])
  return(length(active_code_ids) > 0)
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
#' Applies the process_column_dependencies function to each row's depend_cols
#' and domain values, transforming the depend_cols into a structured format.
#'
#' @param data UI data with depend_cols to process
#'
#' @return Data with processed depend_cols column
process_depend_cols <- function(data) {
  results <- purrr::pmap(
    data[, .(type, depend_cols, domain, outputs)],
    process_column_dependencies
  )
  data[, depend_cols := results]
  return(data)
}

#' Process Column Dependencies
#'
#' @description
#' Transforms column dependencies into a structured data table with domain information.
#'
#' @details
#' Processes a list of column dependencies, handling both domain-prefixed
#' elements (containing dots) and elements without domain prefixes. Creates a data table
#' with column names, domains, and domain types. Special handling is provided for:
#' - col_copy actions that depend on themselves
#' - col_compute actions with no dependencies
#' - Pre-structured data.frame inputs
#'
#' @param type Character string specifying the action type (e.g., "col_copy", "col_compute").
#' @param depend_cols List of column dependencies to process, can be a list of strings
#'   or a data.frame with existing structure.
#' @param domain Character string specifying the current domain value to use for
#'   elements without domain prefixes.
#' @param outputs List of outputs from the action, used for col_copy self-dependencies.
#'
#' @return
#' Data table with processed dependencies containing columns: column_name, domain,
#' and domain_type. Returns empty data table if no valid dependencies are found.
#'
process_column_dependencies <- function(type, depend_cols, domain, outputs) {
  # col_copy actions have dependencies on themselves
  if (type == "col_copy") {
    return(
      data.table::data.table(
        column_name = outputs,
        domain = domain,
        domain_type = classify_data_domains(domain)
      )
    )
  } else if (type == "col_compute" && all(is.na(depend_cols))) {
    # col_compute actions with no dependencies will have an empty dependency table
    return(
      data.table::data.table(
        column_name = character(0),
        domain = character(0),
        domain_type = character(0)
      )
    )
  }
  # If the depend_cols is already a data.frame then add the domain of each dependency to that data.frame and rename
  if (is.data.frame(depend_cols)) {
    out <- data.table::as.data.table(depend_cols)
    return(
      out[, domain_type := classify_data_domains(domain)] |>
        setnames(
          old = c("column", "domain"),
          new = c("column_name", "domain")
        ) |>
        setcolorder(c("column_name", "domain", "domain_type"))
    )
  }
  # Extract all elements
  elements <- unlist(depend_cols)

  # Process based on element format
  elements_with_dot <- elements[grepl("\\.", elements)]
  elements_without_dot <- elements[!grepl("\\.", elements)]

  # Create result data tables
  result <- list()

  # Process elements with domain prefix (containing dots)
  if (length(elements_with_dot) > 0) {
    domains <- sub("\\.(.*)", "", elements_with_dot)
    columns <- sub("^[^.]*\\.", "", elements_with_dot)
    domain_types <- classify_data_domains(domains)

    result[[1]] <- data.table::data.table(
      column_name = columns,
      domain = domains,
      domain_type = domain_types
    )
  }

  # Process elements without domain prefix
  if (length(elements_without_dot) > 0) {
    result[[length(result) + 1]] <- data.table::data.table(
      column_name = elements_without_dot,
      domain = domain,
      domain_type = classify_data_domains(domain)
    )
  }

  # Combine results
  if (length(result) > 0) {
    return(data.table::rbindlist(result))
  } else {
    return(data.table::data.table(
      column_name = character(0),
      domain = character(0),
      domain_type = character(0)
    ))
  }
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
format_outputs <- function(outputs) {
  ifelse(
    !is.na(outputs),
    lapply(outputs, function(x) paste0(unlist(x), collapse = "-")),
    ""
  )
}

#' Format code ID for node ID
#' @description
#' Formats code IDs for inclusion in node IDs.
#' @details
#' Prepends a hyphen to non-NA code IDs, or returns an empty string for NA values.
#'
#' @param code_id Vector of code IDs to format
#'
#' @return Vector of formatted code ID strings
format_code_id <- function(code_id) {
  ifelse(!is.na(code_id), paste0("-", code_id), "")
}

#' Format parameters for node ID
#' @description
#' Formats parameters for inclusion in node IDs.
#' @details
#' Prepends a hyphen to non-NA parameters, or returns an empty string for NA values.
#'
#' @param parameters Vector of parameters to format
#'
#' @return Vector of formatted parameter strings
format_parameters <- function(parameters) {
  ifelse(!is.na(parameters), paste0("-", parameters), "")
}
