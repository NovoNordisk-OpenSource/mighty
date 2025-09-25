#' Validate JSON string against schema (simplified)
#'
#' @param json_data JSON string from the YAML
#' @param schema_path Path to the JSON schema file
#' @param yaml_file Path to the YAML file (for context)
#' @param verbose Verbose output
#' @param validator Optional compiled validator function for caching
#' @return Invisible TRUE or aborts with formatted errors
#' @noRd
validate_schema <- function(
  json_data,
  schema_path,
  yaml_file,
  verbose = TRUE,
  validator = NULL
) {
  is_valid <- if (is.null(validator)) {
    jsonvalidate::json_validate(
      json_data,
      schema_path,
      verbose = TRUE,
      engine = "ajv"
    )
  } else {
    validator(json_data)
  }

  if (!isTRUE(is_valid)) {
    handle_validation_errors(is_valid, yaml_file, verbose)
  }

  invisible(TRUE)
}


#' Handle validation errors (simplified)
#'
#' @param is_valid Result from json_validate() with "errors" attribute
#' @param yaml_file Path to the YAML file (for context)
#' @param verbose Verbose output
#' @noRd
handle_validation_errors <- function(is_valid, yaml_file, verbose) {
  errors <- attr(is_valid, "errors")

  error_info <- format_errors(errors, yaml_file)

  if (is.null(error_info$messages)) {
    cli::cli_abort(c(
      "x" = error_info$header,
      "!" = error_info$details
    ), class = "validation_error")
  } else {
    error_messages <- error_info$messages
    names(error_messages) <- rep("*", length(error_messages))
    cli::cli_abort(c(
      "x" = error_info$header,
      error_messages
    ), class = "validation_error")
  }
}


#' Format a single validation error with enhanced messages
#' @param error_row Single row from validation errors
#' @return Formatted error message string
#' @noRd
format_validation_error <- function(error_row) {
  # Keep the excellent path formatting

  path <- format_error_path(error_row$instancePath, error_row)

  # Use template-based formatting
  message <- format_error_message(error_row)

  glue::glue("{path}: {message}")
}

#' Format error message using templates
#' @param error_row Single error row with keyword, params, etc.
#' @return Formatted message string
#' @noRd
format_error_message <- function(error_row) {
  keyword <- error_row$keyword
  params <- extract_error_params(error_row)

  enhanced_messages <- list(
    required = "Required field '{params$missingProperty}' is missing. Please add this field.",
    additionalProperties = "Unexpected field '{params$additionalProperty}' found. Remove this field or check for typos.",
    type = "Expected type '{params$type}' but got {typeof(error_row$data)}. Please check the data type.",
    pattern = "String does not match required pattern defined by the schema. Check schema",
    minItems = "Array must have at least {params$limit} items. Currently has {length(error_row$data)} items.",
    minLength = "String must be at least {params$limit} characters long. Current length: {nchar(as.character(error_row$data))}.",
    oneOf = "Value matches multiple schemas or none at all. Please check the allowed formats."
  )

  template <- enhanced_messages[[keyword]] %||% error_row$message

  # Handle missing parameters gracefully
  tryCatch({
    glue::glue(template)
  }, error = function(e) {
    # Fallback to basic message if template fails
    error_row$message %||% paste("Validation error:", keyword)
  })
}

#' Extract parameters from error row
#' @param error_row Error row from ajv validation
#' @return List of extracted parameters
#' @noRd
extract_error_params <- function(error_row) {
  if (!"params" %in% names(error_row) || is.null(error_row$params)) {
    return(list())
  }

  params <- error_row$params

  # Handle both data.frame and list params
  if (is.data.frame(params)) {
    as.list(params[1, , drop = FALSE])
  } else if (is.list(params)) {
    params
  } else {
    list()
  }
}

#' Format error path with enhancement (keep the valuable logic)
#' @param path Instance path from ajv
#' @param error_row Error row for context
#' @return Enhanced, human-readable path
#' @noRd
format_error_path <- function(path, error_row = NULL) {

  base <- if (is.na(path) || path == "") "Root level" else enhance_path_readability(path, error_row)
  # Append offending property when AJV provides it
  offender <- extract_offending_member(error_row)
  if (!is.null(offender)) {
    base <- paste0(base, " -> ", offender)
  }

  glue::glue("Error location: {base} | Error message")
}
#' Format validation errors with header and messages (simplified)
#' @noRd
format_errors <- function(errors, yaml_file) {
  if (is.null(errors) || NROW(errors) == 0) {
    return(list(
      header = glue::glue("YAML validation failed for {basename(yaml_file)}"),
      details = "No specific error details available"
    ))
  }

  messages <- purrr::map_chr(seq_len(NROW(errors)), function(i) {
    format_validation_error(errors[i, , drop = FALSE])
  })

  list(
    header = glue::glue("YAML validation failed for {basename(yaml_file)}"),
    messages = messages
  )
}

# Removed - replaced by format_validation_error()


# === Path Enhancement Functions (keep the valuable logic) ===
# These functions provide the excellent path formatting that users appreciate



#' @noRd
enhance_path_readability <- function(path, error_row = NULL) {

  if (grepl("/\\d+(?:/|$)", path)) {
    return(enhance_array_indices(path, error_row))
  }

  clean_path_formatting(path)
}

#' @noRd
enhance_array_indices <- function(path, error_row) {
  pattern <- "(/[^/]+)/(\\d+)"
  enhanced_path <- path
  matches <- gregexpr(pattern, path, perl = TRUE)
  match_data <- matches[[1]]

  if (match_data[1] != -1) {
    for (i in length(match_data):1) {

      match_start <- match_data[i]
      match_length <- attr(match_data, "match.length")[i]
      segment <- substr(path, match_start, match_start + match_length - 1)
      parts <- strsplit(segment, "/")[[1]]
      array_name <- parts[2]
      array_index <- as.numeric(parts[3])

      identifier <- find_array_element_identifier(error_row, array_name, array_index)

      if (!is.null(identifier)) {
        replacement <- paste0("/", array_name, "/", identifier)
      } else {
        human_index <- array_index + 1
        replacement <- paste0("/", array_name, "/item ", human_index)
      }

      enhanced_path <-  paste0(
        substr(enhanced_path, 1, match_start - 1),
        replacement,
        substr(enhanced_path, match_start + match_length, nchar(enhanced_path))
      )
    }
  }

  clean_path_formatting(enhanced_path)
}

#' @noRd
find_array_element_identifier <- function(error_row, array_name, array_index) {
  if (is.null(error_row)) return(NULL)
  identifier_fields <- c("id", "column", "name", "key", "type", "field", "variable")

  for (field in identifier_fields) {
    data_field_name <- paste0("data.", field)
    if (data_field_name %in% names(error_row)) {
      value <- error_row[[data_field_name]]
      if (!is.null(value) && !is.na(value) && nchar(as.character(value)) > 0) {
        return(as.character(value))
      }
    }
  }

  if ("data" %in% names(error_row) && is.list(error_row$data)) {
    data_obj <-  error_row$data
    for (field in identifier_fields) {
      if (field %in% names(data_obj)) {
        value <- data_obj[[field]]
        if (!is.null(value) && !is.na(value) && nchar(as.character(value)) > 0) {
          return(as.character(value))
        }
      }
    }
  }

  NULL
}

#' @noRd
clean_path_formatting <- function(path) {
  clean <- sub("^/", "", path)
  gsub("/", " -> ", clean)
}

#' Extract offending member name (property) from AJV error params
#' @param error_row Single error row
#' @return The offending member/property name or NULL
#' @noRd
extract_offending_member <-  function(error_row) {
  if (is.null(error_row)) return(NULL)

  params <- extract_error_params(error_row)

  # Try common AJV params that carry the interesting name
  candidates <- c(
    params$propertyName,
    params$additionalProperty,
    params$missingProperty
  )

  for (val in candidates) {
    if (!is.null(val) && !is.na(val) && nzchar(as.character(val))) {
      return(as.character(val))
    }
  }

  # Fallback: sometimes wrappers may propagate these at top-level columns
  for (nm in c("propertyName", "additionalProperty", "missingProperty")) {
    if (nm %in% names(error_row)) {
      val <- error_row[[nm]]
      if (!is.null(val) && !is.na(val) && nzchar(as.character(val))) {
        return(as.character(val))
      }
    }
  }

  NULL
}

#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x
