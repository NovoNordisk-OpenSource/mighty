#' Create a Consolidated Environment
#' @description Combines functions from specified packages and source files into
#'   a new environment, checking for duplicates.
#'
#' @details This function loads packages and source files, extracting functions
#'   defined in those sources matching specified `code_ids`. It verifies the
#'   uniqueness of function names across different sources.
#'
#' @param packages Optional character vector specifying package names to load.
#' @param source_files Optional character vector specifying file paths to load R
#'   scripts from.
#' @param code_ids Mandatory character vector containing function names to be
#'   consolidated.
#'
#' @returns A new environment containing the loaded functions.
create_consolidated_env <- function(
  standards_lib = NULL,
  code_ids
) {
 
  code_components <- standards_lib |> 
    lapply(mighty.standards::list_standards) |>
    unlist(use.names = TRUE)
browser()
  assert_all_code_id_sources_exist(code_components, code_ids)
  assert_no_duplicate_fn_names(code_components)

  # This assertion is done after all loading, so that if there are multiple
  # instances of duplication, all will be reported in the error message.
  # Otherwise it might cause the user to iterate until no more duplicates were
  # found

  return(consolidated_env)
}


#' Assert No Duplicate Function Names
#' @description Checks for duplicate function names across loaded packages and
#'   source files, and raises an error if duplicates are found.
#'
#' @details Examines the `function_sources` list to identify conflicts. It aggregates
#'   names from package and file sources, then detects duplicates. The function stops
#'   execution with an informative error detailing the conflicting sources. Requires
#'   the data.table package.
#'
#' @param x List containing function sources divided into `pkg` and `file`
#'   entries, each showing origins of the loaded functions.
#'
#' @returns Returns TRUE invisibly if no duplicates are found, otherwise raises an
#'   error with details about duplicates.
assert_no_duplicate_fn_names <- function(x) {
  duplicate_logical <- duplicated(x)
  if (!any(duplicate_logical)) {
    return(invisible(TRUE))
  }

  dup_index <- duplicate_logical | duplicated(x, fromLast = TRUE)
  duplicated_items <- x[dup_index]
  duplicate_info <- split(names(duplicated_items), duplicated_items)

  error_lines <- purrr::map_chr(names(duplicate_info), function(dup_name) {
    sources <- duplicate_info[[dup_name]]
    paste0(
      "  • {.val ",
      dup_name,
      "} appears in: ",
      paste(paste0("{.path ", sources, "}"), collapse = ", ")
    )
  })

  cli::cli_abort(
    c(
      "Duplicate code components detected:",
      error_lines,
      "i" = "Component names must be unique across all sources"
    )
  )
}

#' Assert All Code ID Sources Exist
#' @description Verifies that all specified code IDs are found in the loaded
#'   function sources, raising an error if any are missing.
#'
#' @param x List containing function sources divided into `pkg` and `file`
#'   entries, each containing details of loaded functions with names as keys.
#' @param code_ids Character vector containing function names that should be
#'   present in the sources.
#'
#' @returns Returns TRUE invisibly if all code IDs are found, otherwise raises
#'   an error with details about missing functions.
assert_all_code_id_sources_exist <- function(code_components, code_ids) {
  missing_fns <- setdiff(code_ids, code_components)
  if (length(missing_fns) == 0) {
    return(invisible(TRUE))
  }
  available_sources <- unique(names(code_components))
  browser()
  cli::cli_abort(
    c(" " = "",
      "Missing code components found",
      "✖" = "{cli::qty(length(missing_fns))} The following code_id{?s} {?was/were} not found in any code component source:",
      paste0("• {.fn ", missing_fns, "}"),
      " " = "",
      "i" = "Available code component sources:",
      paste0("• {.pkg ", available_sources, "}"),
      " " = "",
      " " = "Please ensure these components are either:",
      "*" = "Part of a standard component package, or",
      "*" = "Defined in one of the code component source files as an R function"
    )
  )
}
