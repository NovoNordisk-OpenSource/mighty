#' Validation Error Handler Class
#' 
#' Encapsulates error formatting logic with extensible formatter registry
ValidationErrorHandler <- R6::R6Class("ValidationErrorHandler",
  public = list(
    initialize = function() {
      private$setup_formatters()
    },
    
    format_errors = function(errors, yaml_file) {
      sc_format_errors(errors, yaml_file, self$format_single_error)
    },
    
    format_single_error = function(error_row) {
      sc_format_single_error(error_row, private$formatters)
    }
  ),
  
  private = list(
    formatters = list(),
    
    setup_formatters = function() {
      private$formatters <- sc_setup_formatters(
        private$extract_param,
        private$format_path,
        private$extract_data_preview
      )
    },
    
    format_required_error = function(error_row) {
      sc_format_required_error(error_row, private$extract_param, private$format_path)
    },
    
    format_additional_properties_error = function(error_row) {
      sc_format_additional_properties_error(error_row, private$extract_param, private$format_path)
    },
    
    format_type_error = function(error_row) {
      sc_format_type_error(error_row, private$extract_param, private$format_path)
    },
    
    format_pattern_error = function(error_row) {
      sc_format_pattern_error(error_row, private$extract_param, private$format_path)
    },
    
    format_min_items_error = function(error_row) {
      sc_format_min_items_error(error_row, private$extract_param, private$format_path)
    },
    
    format_min_length_error = function(error_row) {
      sc_format_min_length_error(error_row, private$extract_param, private$format_path)
    },
    
    format_one_of_error = function(error_row) {
      sc_format_one_of_error(error_row, private$format_path)
    },
    
    format_generic_error = function(error_row) {
      sc_format_generic_error(error_row, private$format_path, private$extract_data_preview)
    },
    
    format_path = function(path) {
      sc_format_path(path)
    },
    
    extract_param = function(error_row, param_name) {
      sc_extract_param(error_row, param_name)
    },
    
    extract_data_preview = function(error_row) {
      sc_extract_data_preview(
        error_row,
        private$format_dataframe_preview,
        private$format_vector_preview,
        private$format_atomic_preview
      )
    },
    
    format_dataframe_preview = function(data_content) {
      sc_format_dataframe_preview(data_content)
    },
    
    format_vector_preview = function(data_content) {
      sc_format_vector_preview(data_content)
    },
    
    format_atomic_preview = function(data_content) {
      sc_format_atomic_preview(data_content)
    }
  )
)

# Helper functions ------------------------

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Format validation errors with header and messages
sc_format_errors <- function(errors, yaml_file, format_single_fn) {
  if (nrow(errors) == 0) {
    return(list(
      header = glue::glue("YAML validation failed for {basename(yaml_file)}"),
      details = "No specific error details available"
    ))
  }
  
  messages <- purrr::map_chr(seq_len(nrow(errors)), function(i) {
    format_single_fn(errors[i, ])
  })
  
  list(
    header = glue::glue("YAML validation failed for {basename(yaml_file)}"),
    messages = messages
  )
}

#' Route error to appropriate formatter based on keyword
sc_format_single_error <- function(error_row, formatters) {
  keyword <- error_row$keyword %||% "generic"
  formatter <- formatters[[keyword]] %||% formatters[["generic"]]
  
  formatter(error_row)
}

#' Format missing required field errors
sc_format_required_error <- function(error_row, extract_param_fn, format_path_fn) {
  missing_prop <- extract_param_fn(error_row, "missingProperty")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.na(missing_prop)) {
    glue::glue("{path}: Missing required field '{missing_prop}'")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format additional properties errors
sc_format_additional_properties_error <- function(error_row, extract_param_fn, format_path_fn) {
  additional_prop <- extract_param_fn(error_row, "additionalProperty")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.null(additional_prop) && !is.na(additional_prop)) {
    glue::glue("{path}: Property '{additional_prop}' is not allowed")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format type mismatch errors
sc_format_type_error <- function(error_row, extract_param_fn, format_path_fn) {
  expected_type <- extract_param_fn(error_row, "type")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.null(expected_type) && !is.na(expected_type)) {
    glue::glue("{path}: Expected type '{expected_type}', {error_row$message}")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format pattern validation errors
sc_format_pattern_error <- function(error_row, extract_param_fn, format_path_fn) {
  pattern <- extract_param_fn(error_row, "pattern")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.null(pattern) && !is.na(pattern)) {
    glue::glue("{path}: Value must match pattern '{pattern}'")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format minimum items errors
sc_format_min_items_error <- function(error_row, extract_param_fn, format_path_fn) {
  min_items <- extract_param_fn(error_row, "limit")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.null(min_items) && !is.na(min_items)) {
    glue::glue("{path}: Array must have at least {min_items} items")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format minimum length errors
sc_format_min_length_error <- function(error_row, extract_param_fn, format_path_fn) {
  min_length <- extract_param_fn(error_row, "limit")
  path <- format_path_fn(error_row$instancePath)
  
  if (!is.null(min_length) && !is.na(min_length)) {
    glue::glue("{path}: String must be at least {min_length} characters long")
  } else {
    glue::glue("{path}: {error_row$message}")
  }
}

#' Format oneOf schema errors
sc_format_one_of_error <- function(error_row, format_path_fn) {
  path <- format_path_fn(error_row$instancePath)
  glue::glue("{path}: Value must match exactly one of the allowed schemas")
}

#' Format generic validation errors with context
sc_format_generic_error <- function(error_row, format_path_fn, extract_data_preview_fn) {
  path <- format_path_fn(error_row$instancePath)
  message_parts <- c()
  
  if (!is.na(error_row$message)) {
    message_parts <- c(message_parts, error_row$message)
  }
  
  if (!is.na(error_row$keyword)) {
    message_parts <- c(message_parts, glue::glue("(Rule: {error_row$keyword})"))
  }
  
  # Add data preview for certain error types
  if (!is.na(error_row$keyword) && !error_row$keyword %in% c("additionalProperties", "required")) {
    data_preview <- extract_data_preview_fn(error_row)
    if (!is.null(data_preview)) {
      message_parts <- c(message_parts, glue::glue("Found: {data_preview}"))
    }
  }
  
  final_message <- if (length(message_parts) == 0) {
    "Validation error"
  } else {
    paste(message_parts, collapse = " | ")
  }
  
  glue::glue("{path}: {final_message}")
}

#' Format instance path for error messages
sc_format_path <- function(path) {
  if (is.na(path) || path == "") "Root level" else glue::glue("Path: {path}")
}

#' Extract parameter from error params
sc_extract_param <- function(error_row, param_name) {
  if (!"params" %in% names(error_row) || is.null(error_row$params)) {
    return(if (param_name == "missingProperty") NA else NULL)
  }
  
  params <- error_row$params
  
  # Handle data frame params
  if (is.data.frame(params) && param_name %in% names(params)) {
    value <- params[[param_name]][1]
    return(if (!is.na(value)) value else if (param_name == "missingProperty") NA else NULL)
  }
  
  # Handle list params
  if (is.list(params) && param_name %in% names(params)) {
    return(params[[param_name]])
  }
  
  return(if (param_name == "missingProperty") NA else NULL)
}

#' Extract and format data preview from error
sc_extract_data_preview <- function(error_row, format_dataframe_preview_fn, format_vector_preview_fn, format_atomic_preview_fn) {
  if (!"data" %in% names(error_row) || is.null(error_row$data)) {
    return(NULL)
  }
  
  data_content <- error_row$data
  
  if (is.data.frame(data_content)) {
    return(format_dataframe_preview_fn(data_content))
  } else if (is.vector(data_content)) {
    return(format_vector_preview_fn(data_content))
  } else if (is.atomic(data_content) && length(data_content) == 1) {
    return(format_atomic_preview_fn(data_content))
  } else {
    return(glue::glue("{class(data_content)[1]} object"))
  }
}

#' Format data frame preview for error messages
sc_format_dataframe_preview <- function(data_content) {
  meaningful_cols <- intersect(names(data_content),
                              c("column", "value", "name", "id", "key", "type"))
  
  if (length(meaningful_cols) > 0) {
    for (col in meaningful_cols) {
      values <-  data_content[[col]]
      non_na_values <- values[!is.na(values)]
      if (length(non_na_values) > 0) {
        preview <-  paste(non_na_values, collapse = ", ")
        return(substr(preview, 1, 100))
      }
    }
  }
  
  glue::glue("data.frame with {nrow(data_content)} row(s) and {ncol(data_content)} column(s)")
}

#' Format vector preview for error messages
sc_format_vector_preview <- function(data_content) {
  non_na_content <- data_content[!is.na(data_content)]
  if (length(non_na_content) == 0) {
    return(NULL)
  }
  
  preview <- paste(non_na_content, collapse = ", ")
  if (nchar(preview) > 100) {
    preview <-  paste0(substr(preview, 1, 97), "...")
  }
  preview
}

#' Format atomic value preview for error messages
sc_format_atomic_preview <- function(data_content) {
  if (is.na(data_content)) {
    return(NULL)
  }
  substr(as.character(data_content), 1, 100)
}

#' Set up error formatters registry
sc_setup_formatters <- function(extract_param_fn, format_path_fn, extract_data_preview_fn) {
  list(
    required = function(error_row) {
      sc_format_required_error(error_row, extract_param_fn, format_path_fn)
    },
    additionalProperties = function(error_row) {
      sc_format_additional_properties_error(error_row, extract_param_fn, format_path_fn)
    },
    type = function(error_row) {
      sc_format_type_error(error_row, extract_param_fn, format_path_fn)
    },
    pattern = function(error_row) {
      sc_format_pattern_error(error_row, extract_param_fn, format_path_fn)
    },
    minItems = function(error_row) {
      sc_format_min_items_error(error_row, extract_param_fn, format_path_fn)
    },
    minLength = function(error_row) {
      sc_format_min_length_error(error_row, extract_param_fn, format_path_fn)
    },
    oneOf = function(error_row) {
      sc_format_one_of_error(error_row, format_path_fn)
    },
    generic = function(error_row) {
      sc_format_generic_error(error_row, format_path_fn, extract_data_preview_fn)
    }
  )
}

# UTILITY FUNCTIONS--------------------
#' Check if yq is available
check_yq_available <- function() {
  Sys.which("yq") != ""
}

#' Convert YAML file to JSON string using yq
convert_yaml_to_json_with_yq <- function(yaml_file) {
  yq_result <- tryCatch({
    system2("yq",
           args = c("eval", "-o=json", shQuote(yaml_file)),
           stdout = TRUE,
           stderr = TRUE,
           wait = TRUE)
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to execute yq command",
      "!" = "Error: {e$message}"
    ))
  })
  
  exit_code <- attr(yq_result, "status")
  
  if (!is.null(exit_code) && exit_code != 0) {
    error_msg <- if (length(yq_result) > 0) {
      paste(yq_result, collapse = "\n")
    } else {
      glue::glue("yq returned exit code: {exit_code}")
    }
    
    cli::cli_abort(c(
      "x" = "Failed to parse YAML file {.file {basename(yaml_file)}} with yq",
      "!" = error_msg
    ))
  }
  
  if (length(yq_result) == 0) {
    cli::cli_abort(c(
      "x" = "yq produced no output for {.file {basename(yaml_file)}}"
    ))
  }
  
  json_data <- paste(yq_result, collapse = "\n")
  
  if (nchar(trimws(json_data)) == 0) {
    cli::cli_abort(c(
      "x" = "yq produced empty JSON output for {.file {basename(yaml_file)}}"
    ))
  }
  
  return(json_data)
}

#' Convert YAML to JSON using R yaml package
convert_yaml_to_json_with_r <- function(yaml_file) {
  yaml_data <- tryCatch({
    yaml::read_yaml(
      yaml_file,
      handlers = list(seq = function(x) as.list(x))
    )
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to parse YAML file {.file {basename(yaml_file)}}",
      "!" = "YAML syntax error: {e$message}"
    ))
  })
  
  json_data <- tryCatch({
    jsonlite::toJSON(yaml_data, auto_unbox = TRUE, null = "null")
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to convert YAML file {.file {basename(yaml_file)}} to JSON for validation",
      "!" = "Error: {e$message}"
    ))
  })
  
  return(json_data)
}

#' Convert YAML file to JSON string using appropriate method
convert_yaml_to_json <- function(yaml_file, use_yq = TRUE, verbose = TRUE) {
  use_yq_method <- use_yq && check_yq_available()
  
  if (use_yq_method) {
    if (verbose) {
      cli::cli_inform(c("i" = "Using yq for YAML parsing"))
    }
    return(convert_yaml_to_json_with_yq(yaml_file))
  }
  
  if (use_yq && verbose) {
    cli::cli_warn(c("!" = "yq not available, falling back to yaml package"))
  }
  return(convert_yaml_to_json_with_r(yaml_file))
}

#' Parse YAML file for return value
parse_yaml_for_return <- function(yaml_file) {
  tryCatch({
    yaml::read_yaml(yaml_file)
  }, error = function(e) {
    cli::cli_abort(c(
      "x" = "Failed to parse YAML file for return value",
      "!" = "Error: {e$message}"
    ))
  })
}

#' Get schema path from schema name
get_schema_path <- function(schema_name) {
  if (missing(schema_name)) {
    available <- list_available_schemas()
    cli::cli_abort(c(
      "x" = "Schema name is required",
      "i" = "Available schemas: {.val {available}}"
    ))
  }
  
  schema_path <- system.file("schemas", paste0(schema_name, ".json"),
                            package = "mighty")
  
  if (schema_path == "") {
    available <- list_available_schemas()
    cli::cli_abort(c(
      "x" = "Schema not found: {.val {schema_name}}",
      "i" = "Available schemas: {.val {available}}"
    ))
  }
  
  return(schema_path)
}

#' List available schemas
list_available_schemas <- function() {
  schemas_dir <- system.file("schemas", package = "mighty")
  
  if (schemas_dir == "") {
    return(character(0))
  }
  
  schema_files <- list.files(schemas_dir, pattern = "\\.json$", full.names = FALSE)
  sub("\\.json$", "", schema_files)
}

#' Validate JSON string against schema
validate_json_against_schema <- function(json_data, schema_path, yaml_file, verbose = TRUE, error_handler = NULL) {
  # TODO: create a json_validator in the parent read_adam_domain section to avoid re-parsing the schema inside each iteration
  is_valid <- jsonvalidate::json_validate(json_data, schema_path, verbose = TRUE, engine = "ajv")
  
  if (!is_valid) {
    handle_validation_errors(is_valid, yaml_file, verbose, error_handler)
  }
}

#' Handle validation errors with informative messages
handle_validation_errors <- function(is_valid, yaml_file, verbose, error_handler = NULL) {
  errors <- attr(is_valid, "errors")
  
  if (is.null(error_handler)) {
    error_handler <- ValidationErrorHandler$new()
  }
  
  error_info <- error_handler$format_errors(errors, yaml_file)
  
  if (is.null(error_info$messages)) {
    cli::cli_abort(c(
      "x" = error_info$header,
      "!" = error_info$details
    ))
  } else {
    error_messages <- error_info$messages
    names(error_messages) <- rep("*", length(error_messages))
    
    cli::cli_abort(c(
      "x" = error_info$header,
      error_messages
    ))
  }
}

#' Validate YAML configuration file against JSON schema
#'
#' @param yaml_file Path to YAML configuration file
#' @param schema_name Name of schema to use for validation
#' @param verbose Show detailed validation messages (default: TRUE)
#' @param use_yq Use yq for YAML parsing (default: TRUE, falls back to yaml package if FALSE or yq unavailable)
#' @return Parsed YAML data if valid, otherwise throws error
#' @export
#' @examples
#' \dontrun{
#' # Validate against adam_domain schema
#' config <- validate_yaml("my_adam_config.yml", "adam_domain")
#'
#' # Quiet validation
#' config <- validate_yaml("my_config.yml", "adam_domain", verbose = FALSE)
#'
#' # Force use of yaml package instead of yq
#' config <- validate_yaml("my_config.yml", "adam_domain", use_yq = FALSE)
#' }
validate_yaml <- function(yaml_file, schema_name, verbose = TRUE, use_yq = TRUE) {
  if (!file.exists(yaml_file)) {
    cli::cli_abort(c(
      "x" = "YAML file not found: {.file {yaml_file}}"
    ))
  }
  
  schema_path <- get_schema_path(schema_name)
  
  if (verbose) {
    cli::cli_inform(c(
      "i" = "Validating {.file {basename(yaml_file)}} against {.val {schema_name}} schema"
    ))
  }
  
  json_data <- convert_yaml_to_json(yaml_file, use_yq, verbose)
  
  error_handler <- ValidationErrorHandler$new()
  
  validate_json_against_schema(json_data, schema_path, yaml_file, verbose, error_handler)
  
  yaml_data <- parse_yaml_for_return(yaml_file)
  
  if (verbose) {
    cli::cli_alert_success("YAML file {.file {basename(yaml_file)}} is valid!")
  }
  
  return(invisible(yaml_data))
}
