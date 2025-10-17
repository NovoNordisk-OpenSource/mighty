#' Validate YAML configuration file against JSON schema
#'
#' @param yaml_file Path to YAML configuration file
#' @param schema_name Name of schema to use for validation
#' @param verbose Show detailed validation messages (default: TRUE)
#' @param use_yq Use yq for YAML parsing (default: TRUE, falls back to yaml package if FALSE or yq unavailable)
#' @return Parsed YAML data if valid, otherwise throws error
#' @export
validate_yaml <- function(
  yaml_file,
  schema_name,
  verbose = TRUE,
  use_yq = TRUE
) {
  if (!file.exists(yaml_file)) {
    cli::cli_abort(c("x" = "YAML file not found: {.file {yaml_file}}"))
  }

  schema_path <- get_schema_path(schema_name)

  if (verbose) {
    cli::cli_inform(c(
      "i" = "Validating {.file {basename(yaml_file)}} against {.val {schema_name}} schema"
    ))
  }

  json_data <- convert_yaml_to_json(yaml_file, use_yq, verbose)

  # Schema validation with simplified formatting
  validate_schema(
    json_data = json_data,
    schema_path = schema_path,
    yaml_file = yaml_file,
    verbose = verbose
  )

  yaml_data <- parse_yaml_for_return(yaml_file)

  # Business rules unchanged (still functional)
  validate_business_logic(yaml_data, yaml_file, ruleset_name = "adam_domain")

  if (verbose) {
    cli::cli_alert_success("YAML file {.file {basename(yaml_file)}} is valid!")
  }

  invisible(yaml_data)
}


#' Check if yq is available
check_yq_available <- function() {
  Sys.which("yq") != ""
}

#' Convert YAML File to JSON String Using yq
#'
#' @description
#' Converts a YAML file to JSON string using the yq command-line tool with
#' comprehensive error handling.
#'
#' @param yaml_file Path to the YAML file to convert.
#'
#' @return
#' Character string containing the JSON representation of the YAML content.
#'
convert_yaml_to_json_with_yq <- function(yaml_file) {
  yq_result <- tryCatch(
    {
      system2(
        "yq",
        args = c("eval", "-o=json", shQuote(yaml_file)),
        stdout = TRUE,
        stderr = TRUE,
        wait = TRUE
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to execute yq command",
        "!" = "Error: {e$message}"
      ))
    }
  )

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

#' Convert YAML to JSON Using R yaml Package
#'
#' @description
#' Converts a YAML file to JSON string using R's yaml and jsonlite packages
#' with error handling.
#'
#' @param yaml_file Path to the YAML file to convert.
#'
#' @return
#' Character string containing the JSON representation of the YAML content.
#'

convert_yaml_to_json_with_r <- function(yaml_file) {
  yaml_data <- tryCatch(
    {
      yaml::read_yaml(
        yaml_file,
        handlers = list(seq = function(x) as.list(x))
      )
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to parse YAML file {.file {basename(yaml_file)}}",
        "!" = "YAML syntax error: {e$message}"
      ))
    }
  )

  json_data <- tryCatch(
    {
      jsonlite::toJSON(yaml_data, auto_unbox = TRUE, null = "null")
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to convert YAML file {.file {basename(yaml_file)}} to JSON for validation",
        "!" = "Error: {e$message}"
      ))
    }
  )

  return(json_data)
}

#' Convert YAML File to JSON String
#'
#' @description
#' Converts a YAML file to JSON format using yq command-line tool or R's yaml
#' package as fallback.
#'
#' @param yaml_file Path to the YAML file to convert.
#' @param use_yq Logical, whether to prefer yq tool when available (default TRUE).
#' @param verbose Logical, whether to display conversion method messages (default TRUE).
#'
#' @return
#' Character string containing the JSON representation of the YAML content.
#'
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

#' Parse YAML File for Return Value
#'
#' @description
#' Reads and parses a YAML file into an R object with error handling.
#'
#' @param yaml_file Path to the YAML file to parse.
#'
#' @return
#' Parsed YAML content as an R object (list, vector, etc.).
#'
parse_yaml_for_return <- function(yaml_file) {
  tryCatch(
    {
      yaml::read_yaml(yaml_file)
    },
    error = function(e) {
      cli::cli_abort(c(
        "x" = "Failed to parse YAML file for return value",
        "!" = "Error: {e$message}"
      ))
    }
  )
}

#' Get Schema Path from Schema Name
#'
#' @description
#' Retrieves the full file path to a JSON schema file by name with validation.
#'
#' @param schema_name Character string specifying the schema name (without .json extension).
#'
#' @return
#' Character string containing the full path to the schema file.
#'
get_schema_path <- function(schema_name) {
  if (missing(schema_name)) {
    available <- list_available_schemas()
    cli::cli_abort(c(
      "x" = "Schema name is required",
      "i" = "Available schemas: {.val {available}}"
    ))
  }

  schema_path <- system.file(
    "schemas",
    paste0(schema_name, ".json"),
    package = "mighty"
  )

  if (schema_path == "") {
    available <- list_available_schemas()
    cli::cli_abort(c(
      "x" = "Schema not found: {.val {schema_name}}",
      "i" = "Available schemas: {.val {available}}"
    ))
  }

  return(schema_path)
}

#' List Available Schemas
#'
#' @description
#' Returns a vector of available schema names from the package's schemas directory.
#'
#' @return
#' Character vector of schema names (without .json extension), or empty vector
#' if no schemas are found.
#'
list_available_schemas <- function() {
  schemas_dir <- system.file("schemas", package = "mighty")

  if (schemas_dir == "") {
    return(character(0))
  }

  schema_files <- list.files(
    schemas_dir,
    pattern = "\\.json$",
    full.names = FALSE
  )
  sub("\\.json$", "", schema_files)
}
