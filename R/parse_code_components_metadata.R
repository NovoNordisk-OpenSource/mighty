#' Extract column metadata from {mighty} code components across packages and
#' source files
#' @description Retrieves metadata about ADaM columns that functions depend on
#' and produce by parsing Roxygen documentation from either installed packages
#' or R source files.
#'
#' @details
#' Serves as the main entry point for extracting column dependency
#' and output information from R functions. It can extract this metadata from
#' two sources:
#' 1. Installed R packages (using their compiled documentation)
#' 2. R source files (parsing the Roxygen comments directly)
#'
#' The function is needed to transform the information required for constructing
#' the topology into a format that mighty can ingest
#'
#' @param pkgs Character vector of package names to extract metadata from
#' @param source_files Character vector of file paths to R source files
#' @param function_names Character vector of function names to extract metadata
#'   for
#'
#' @return A list where each element is named after a function and contains:
#'   \item{depend_cols}{Character vector of columns the function depends on}
#'   \item{outputs}{Character vector of columns the function creates or modifies}
#'   \item{type}{The function type (e.g., "col_compute" or "row_compute")}
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
return(c(pkg_out, source_file_out))
}

#' Extract dependency metadata from package documentation
#' @description Parses compiled R documentation files (.Rd) from a package to
#' extract column dependency and output information for specified functions.
#'
#' @details Accesses the compiled documentation database of a package using
#' tools::Rd_db() and then extracts metadata about column dependencies and
#' outputs.
#'
#' @param pkg Character string naming the package to extract metadata from
#' @param function_names Character vector of function names to extract metadata
#'   for
#'
#' @return A list where each element is named after a function and contains
#'   metadata about its dependencies and outputs
parse_code_components_metadata_from_package_rd <- function(pkg, function_names) {
  rd_db <- tools::Rd_db(pkg)
  out <- lapply(function_names,
                parse_code_component_i,
                rd_db = rd_db,
                pkg = pkg) |>
    unlist(FALSE)
  return(out)
}

#' Extract metadata for a single function from package documentation
#' @description
#' Parses the .Rd documentation for a specific function to extract its
#' column dependencies, outputs, and function type.
#'
#' @details
#' Workhorse for extracting metadata from compiled package
#' documentation. Looks for specific subsections in the .Rd file:
#' - "Depends" subsection for column dependencies
#' - "Outputs" subsection for columns created or modified
#' - "Type" subsection for the function type
#'
#' Silently skips functions that aren't found in the documentation
#' database, as they might be available from another source.
#'
#' @param fn Character string naming the function to extract metadata for
#' @param rd_db An Rd database object returned by tools::Rd_db()
#' @param pkg Character string naming the package (used for reference)
#'
#' @return A list with a single named element (the function name) containing:
#'   \item{depend_cols}{Character vector of columns the function depends on}
#'   \item{outputs}{Character vector of columns the function creates or modifies}
#'   \item{type}{The function type (normalized to mighty's terminology)}
parse_code_component_i <- function(fn, rd_db, pkg) {
  rd_name <- paste0(fn, ".Rd")
  if (!rd_name %in% names(rd_db)) {
    # We don't throw an error/warning here, because the function might be in
    # another source
    return(NULL)
  }

  rd_obj <- rd_db[[rd_name]]

  depend_cols <- rd_obj |>
    find_subsection(heading = "Depends") |>
    exctract_all_items()

  output_cols <- rd_obj |>
    find_subsection(heading = "Outputs") |>
    exctract_all_items()

  type <- rd_obj |> extract_type()
  result <- list()
  result[[fn]] <- list()
  result[[fn]]$depend_cols <- depend_cols |> unlist()
  result[[fn]]$outputs <- output_cols |> unlist()
  result[[fn]]$type <- type
  return(result)
}

#' Extract metadata from R source files
#' @description Parses Roxygen comments directly from R source files to extract
#' column dependency and output information for functions.
#'
#' @details Unlike parsing from package documentation, works with source files
#' directly, making it useful for analyzing functions that haven't been built
#' into a package yet or for development workflows.
#'
#' Uses roxygen2 to parse the file and extract metadata from special tags in the
#' Roxygen comments. Can filter to only process specific functions if
#' function_names is provided.
#'
#' @param file_path Character string with the path to an R source file
#' @param function_names Optional character vector of function names to extract
#'   metadata for. If NULL, metadata for all functions in the file will be
#'   extracted.
#'
#' @return A list where each element is named after a function and contains
#'   metadata about its dependencies and outputs. Functions without relevant
#'   metadata are omitted.
parse_code_components_metadata_from_file <- function(file_path, function_names = NULL) {
  parsed <- roxygen2::parse_file(file_path)
  result <- list()

  for (block in parsed) {
    function_name <- block$object$topic

    # Skip if we're only interested in specific functions and this isn't one of them
    if (!is.null(function_names) && !(function_name %in% function_names)) {
      next
    }

    metadata <- extract_metadata_from_block(block)

    # Only add to results if we found metadata
    if (!is_empty_metadata(metadata)) {
      result[[function_name]] <- metadata
    }
  }

  return(result)
}

#' Extract metadata from a Roxygen comment block
#' @description
#' Processes a parsed Roxygen block to extract column dependencies, outputs,
#' and function type information.
#'
#' @details
#' Examines the tags in a Roxygen block (as parsed by roxygen2::parse_file)
#' and extracts three specific types of metadata:
#'
#' 1. @depends tags: Columns that the function requires as input
#' 2. @outputs tags: Columns that the function creates or modifies
#' 3. @type tags: The function type (normalized to mighty's terminology)
#'
#' Handles the special ".self" dataset notation used in mighty.standards
#' by properly formatting dependencies to ensure consistent representation.
#'
#' @param block A parsed Roxygen block from roxygen2::parse_file
#'
#' @return A list containing:
#'   \item{depend_cols}{Character vector of columns the function depends on}
#'   \item{outputs}{Character vector of columns the function creates or modifies}
#'   \item{type}{The function type (normalized to mighty's terminology)}
extract_metadata_from_block <- function(block) {
  depend_cols <- character(0)
  outputs <- character(0)
  type <- NULL

  for (tag in block$tag) {
    if (tag$tag == "depends") {
      depend_cols <- c(depend_cols, format_dependency(tag$val$name, tag$val$description))
    } else if (tag$tag == "outputs") {
      outputs <- c(outputs, tag$val)
    } else if (tag$tag == "type") {
      type <- normalize_type(tag$val)
    }
  }

  return(list(
    depend_cols = depend_cols,
    outputs = outputs,
    type = type
  ))
}
format_dependency <- function(dataset, column) {
  # The .self nomenclature is used by mighty.standards to force the user
  # to be explicit about the provenance and make automating on top easier,
  # however, this is not use by mighty, so needs to be removed
  if (dataset == ".self") {
    return(column)
  } else {
    return(paste(dataset, column, sep = "."))
  }
}
normalize_type <- function(type_value) {
  # Map "derivation" to "col_compute" and "row" to "row_compute". This is
  # because mighty internally uses different nomenclature that doesn't
  # match with CDISC, so we need to convert to mighy terms here
  switch(type_value,
         "derivation" = "col_compute",
         "row" = "row_compute",
         type_value)
}
is_empty_metadata <- function(metadata) {
  length(metadata$depend_cols) == 0 &&
    length(metadata$outputs) == 0 &&
    is.null(metadata$type)
}

#' Find subsections with a specific heading in an Rd object
#' @description
#' Locates all subsections with a given heading within an R documentation object.
#'
#' @details
#' Searches through an Rd object (parsed R documentation) to find
#' all subsections that match a specified heading. Particularly useful for
#' extracting structured information from documentation that follows a consistent
#' format.
#'
#' Used to locate custom subsections like "Depends", "Outputs", and "Type"
#' that contain the metadata about column dependencies and function behavior.
#'
#' @param rd An Rd object (parsed R documentation)
#' @param heading Character string specifying the subsection heading to find
#'
#' @return A list of Rd subsection objects that match the specified heading.
#'   Returns an empty list if no matching subsections are found.
find_subsection <- function(rd, heading) {
  subsections <- find_tag(rd, "\\subsection")
  # Each subsection will be a list where the first element is the title.
  matches <- Filter(function(subd) {
    txt <- paste(unlist(subd[[1]]), collapse = " ")
    identical(txt, heading)
  }, subsections)
  matches
}

find_tag <- function(node, tag) {
  results <- list()
  if (is.list(node)) {
    # If node has an Rd_tag attribute and it equals the tag, store it.
    if (!is.null(attr(node, "Rd_tag")) && attr(node, "Rd_tag") == tag) {
      results <- c(results, list(node))
    }
    # Recursively search over children
    for (child in node) {
      results <- c(results, find_tag(child, tag))
    }
  }
  results
}

#' Extract items from an itemized list in Rd documentation
#' @description Processes an itemized list from R documentation to extract the
#' text content of each item.
#'
#' @details Parses the structure of an itemized list in Rd documentation to
#' extract the text content of each item. Specifically designed to handle the
#' format used for documenting column dependencies and outputs in ADaM
#' functions.
#'
#' Handles the special ".self" notation used in mighty.standards by removing the
#' ".self." prefix from extracted items, ensuring compatibility with mighty
#'
#' @param node An Rd node containing an itemized list
#'
#' @return A list of character strings, each representing the text content of an
#'   item in the itemized list. Empty or malformed items are skipped.
extract_items <- function(node) {
  items_list <- list()
  node_length <- length(node)
  for (i in seq_len(node_length)) {
    # Skip if the current element is not an "\\item" node.
    if (is.null(attr(node[[i]], "Rd_tag")) || attr(node[[i]], "Rd_tag") != "\\item") next

    # If there is no following sibling (i.e. end of the line)
    if (i >= node_length) next

    next_child <- node[[i + 1]]
    text_val <- next_child |>
      unlist() |>
      paste(collapse = " ") |>
      trimws()

    # The .self nomenclature is used by mighty.standards to force the user to be
    # explicit about the provenance and make automating on top easier, however,
    # this is not use by mighty, so needs to be removed
    text_val <- gsub("^\\.self\\.", "", text_val)


    if (nchar(text_val) == 0) next

    items_list[[length(items_list) + 1]] <- text_val
  }

  items_list
}
exctract_all_items <- function(section) {
  # Initialize an empty list to hold all extracted items
  items_container <- list()

  # Loop through each provided subsection
  for (sec in section) {

    # Find any nested \itemize nodes within the current subsection
    itemize_nodes <- find_tag(sec, "\\itemize")

    # For each itemize node, extract the items and combine them
    for (itemize in itemize_nodes) {

      items <- extract_items(itemize)
      items_container <- c(items_container, items)
    }
  }

  # Return the combined list of dependency items
  return(items_container)
}

extract_type <- function(rd_obj){
  type_0 <- find_subsection(rd_obj, heading = "Type") |> unlist() |> paste0( collapse = "")
  type_1 <- gsub("^Type", "",type_0)
  type_2 <- gsub("\\n", "", type_1)

  # mighty internally uses different nomenclature that doesn't match with
  # CDISC, so we need to convert to mighy terms here
  type_mighty <- switch(tolower(type_2),
                        "derivation"="col_compute",
                        "row" = "row_compute")

  return(type_mighty)

}
