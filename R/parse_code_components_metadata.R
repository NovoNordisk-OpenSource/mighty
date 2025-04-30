#' Parses parent and children ADaM columns from functions' Roxygen header
#' @description
#' The metadata regarding the ADaM columns each function requires to be
#' available to it are stored in the functions roxygen header, along with the
#' specification of which ADaM column(s) the function modifies/creates.
#'
#' @param pkgs
#' @param source_files
#' @param function_names
#'
#' @returns
#' @export
#'
#' @examples
parse_code_components_metadata <- function(pkgs = NULL,
                                           source_files = NULL,
                                           function_names) {
  pkg_out <- NULL
  source_file_out <- NULL

  if (!is.null(pkgs)) {
    pkg_out <- lapply(pkgs,
                      parse_code_components_metadata_from_package_rd,
                      function_names = function_names) |>
      unlist(FALSE)
  }

  if (!is.null(source_files)) {
    source_file_out <- lapply(source_files, parse_code_components_metadata_from_file) |>
      unlist(FALSE)
  }

}

parse_code_components_metadata_from_package_rd <- function(pkg, function_names) {
  rd_db <- tools::Rd_db(pkg)
  browser()
  out <- lapply(function_names,
                parse_code_component_i,
                rd_db = rd_db,
                pkg = pkg) |>
    unlist(FALSE)
  return(out)
}

parse_code_component_i <- function(fn, rd_db, pkg) {
  rd_name <- paste0(fn, ".Rd")
  if (!rd_name %in% names(rd_db)) {
    # We don't throw an error/warning here, because the function might be in
    # another source
    next
  }

  rd_obj <- rd_db[[rd_name]]

  section_tags <- which(vapply(rd_obj, function(x) {
    attr(x, "Rd_tag") == "\\section"
  }, logical(1)))

  if (length(section_tags) == 0) {
    sprintf(
      "Function '%s' in package '%s' does not contain any metadata sections required for this framework to work.",
      fn,
      pkg
    ) |> stop()
  }
  result <- list()
  result[[fn]] <- list()
  # Check each section to find the metadata section
  for (idx in section_tags) {
    section <- rd_obj[[idx]]
    section_name <- as.character(section[[1]])[1]

    if (section_name != "metadata") {
      next
    }

    # The metadata section contains nested elements - we need the second element
    # which holds the actual content (first element is just the section name)
    content <- section[[2]]

    # Rd files store YAML content within 'preformatted' blocks to preserve formatting
    # We need to locate this specific block to extract the structured metadata
    preformatted_idx <- which(vapply(content, function(x) {
      attr(x, "Rd_tag") == "\\preformatted"
    }, logical(1)))

    if (length(preformatted_idx) == 0) {
      return(result)
    }

    # Isolate the YAML text so it can be read-in via yml parser
    yaml_text <- paste(as.character(content[[preformatted_idx]]), collapse = "\n")

    tryCatch({
      metadata <- yaml::read_yaml(text = yaml_text)
    }, error = function(e) {
      stop(paste(
        "Error parsing YAML metadata for function",
        fn,
        ":",
        e$message
      ))

    })

    result[[fn]]$depend_cols <- metadata$depend_cols
    result[[fn]]$outputs <- metadata$outputs
    result[[fn]]$type <- metadata$type

    # This break assumes there is only 1 metadata block per function
    break
  }

  return(result)
}


' Parse metadata from R source files using roxygen2
#'
#' @param file_path Path to the R file
#' @param function_names Names of functions to extract metadata for (if NULL, extract all)
#' @return List of metadata for each function
parse_code_components_metadata_from_file <- function(file_path, function_names = NULL) {
  # Ensure roxygen2 is available

  # Parse the file with roxygen2
  parsed <- suppressWarnings(roxygen2::parse_file(file_path))

  # Initialize results
  result <- list()

  # Process each roxygen block
  for (block in parsed) {
    # Get function name
    fn <- block$object$topic

    # Skip if we're only interested in specific functions and this isn't one of them
    if (!is.null(function_names) && !(fn %in% function_names)) {
      next
    }

    # Find the metadata section in the tags
    metadata_tag <- NULL
    for (tag in block$tag) {
      if (tag$tag == "section" && grepl("^metadata:", tag$raw)) {
        metadata_tag <- tag
        break
      }
    }
    if (is.null(metadata_tag)) {
      next
    }
    # 1. Remove markdown code block delimiters
    metadata_content <- sub("^metadata:\\s*", "", metadata_tag$raw)
    metadata_content <- gsub("```yaml\\s*", "", metadata_content)
    metadata_content <- gsub("```\\s*", "", metadata_content)
    metadata_content <- gsub("#'\\s*", "", metadata_content)
    tryCatch({
      metadata <- yaml::read_yaml(text = metadata_content)
    }, error = function(e) {
      warning(paste(
        "Error parsing YAML metadata for function",
        fn,
        ":",
        e$message
      ))
      # Print the problematic content for debugging
      message("Problematic YAML content:")
      message(metadata_content)
    })

    result[[fn]] <- list(
      depend_cols = metadata$depend_cols,
      outputs = metadata$outputs,
      type = metadata$type
    )
  }
  return(result)
}
