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
create_consolidated_env <- function(packages = NULL,
                                    source_files = NULL,
                                    code_ids) {
  consolidated_env <- new.env(parent = emptyenv())
  if (is.null(code_ids) || length(code_ids) == 0) {
    return(consolidated_env)
  }

  # Dictionary to track function origins
  function_sources <- list(pkg = list(), file = list())

  # Check and load packages
  if (!is.null(packages)) {
    function_sources$pkg <- load_functions_from_packages(packages, code_ids, function_sources, envr = consolidated_env)
  }

  # Check and load source files
  if (!is.null(source_files)) {
    function_sources$file <- load_functions_from_files(source_files, function_sources, envr = consolidated_env)
  }
  assert_all_code_id_sources_exist(function_sources, code_ids)

  # This assertion is done after all loading, so that if there are multiple
  # instances of duplication, all will be reported in the error message.
  # Otherwise it might cause the user to iterate until no more duplicates were
  # found
  assert_no_duplicate_fn_names(function_sources)
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
  files <- x$file |>
    lapply(data.table::rbindlist) |>
    data.table::rbindlist()
  pkgs <- x$pkg |>
    lapply(data.table::rbindlist) |>
    data.table::rbindlist()
  all_fn <- rbind(files, pkgs)
  duplicates <- all_fn[, .N, by = fn][N > 1]$fn

  if (length(duplicates) == 0) {
    return(invisible(TRUE))
  }

  error_messages <- vapply(duplicates, function(func_name) {
    sources <- all_fn[fn == func_name, fn_source]
    paste(
      "Duplicate function '",
      func_name,
      "' found in the following sources:\n",
      paste(sources, collapse = "\n"),
      sep = ""
    )
  }, FUN.VALUE = character(1))

  stop(paste(error_messages, collapse = "\n\n"))
}

#' Assert Source Availability
#' @description Verifies the availability of specified packages or files,
#'   raising an error if they are not available.
#'
#' @details Checks whether a given package is installed and available, or whether
#'   a specified file exists. An error is raised with details if the resource is
#'   not available.
#'
#' @param item Character string specifying the name of the package or file to
#'   check.
#' @param check_type Character string, either "package" or "file", indicating
#'   the type of source to check. Defaults to "package".
#'
#' @returns Returns TRUE invisibly if the specified package is available or the
#'   file exists. Otherwise raises an error
assert_source_available <- function(item, check_type = c("package", "file")) {
  check_type <- match.arg(check_type)
  if (check_type == "package") {
    if (!requireNamespace(item, quietly = TRUE)) {
      stop(sprintf("Error: Package '%s' is not available.", item))
    }
  } else if (check_type == "file") {
    if (!file.exists(item)) {
      stop(sprintf("Error: File '%s' does not exist.", item))
    }
  }
  return(invisible(TRUE))
}

#' Load Functions from Specified Packages
#' @description Loads functions from specified R packages that match the given
#'   `code_ids` and assigns them to a specified environment.
#'
#' @details Iterates over the provided packages, verifies their availability,
#'   and loads functions that match the provided `code_ids`. The functions are
#'   assigned to the environment specified by `envr`, while their origins are
#'   recorded in the `function_sources` list.
#'
#' @param packages Character vector specifying the names of R packages to load
#'   functions from.
#' @param code_ids Character vector containing the names of functions to be
#'   loaded.
#' @param function_sources List used to track the origins of loaded functions,
#'   including package sources.
#' @param envr Environment to which the loaded functions are assigned.
#'
#' @returns The updated `function_sources$pkg` list containing details of
#'   functions loaded from the specified packages.

load_functions_from_packages <- function(packages,
                                         code_ids,
                                         function_sources,
                                         envr) {
  for (pkg in packages) {
    assert_source_available(pkg, check_type = "package")
    function_sources$pkg[[pkg]] <- list()
    pkg_ns <- asNamespace(pkg)
    pkg_exports <- getNamespaceExports(pkg)
    # Only load functions matching code_ids that are exported by the package
    code_ids_in_pkg <- intersect(code_ids, pkg_exports)
    for (fn_name in code_ids_in_pkg) {
      fn <- get(fn_name, envir = pkg_ns)
      if (is.function(fn)) {
        assign(fn_name, fn, envir = envr)
        function_sources$pkg[[pkg]][[fn_name]] <- list(fn = fn_name, fn_source = sprintf("package %s", pkg))
      }
    }
  }
  return(function_sources$pkg)
}
#' Load Functions from Specified Source Files
#' @description Loads functions from specified R script files into a given
#'   environment.
#'
#' @details For each given file, the function checks its existence (raising an
#'   error if not found), sources it into a temporary environment, and assigns
#'   any functions found to the specified environment `envr`. Function origins
#'   are tracked within the `function_sources` list.
#'
#' @param source_files Character vector specifying file paths of R scripts.
#' @param function_sources List used to track the origins of loaded functions,
#'   including file sources.
#' @param envr Environment to which the loaded functions are assigned.
#'
#' @returns The updated `function_sources$file` list containing details of
#'   functions loaded from the specified files.

load_functions_from_files <- function(source_files, function_sources, envr) {
  for (file in source_files) {
    assert_source_available(file, check_type = "file")
    function_sources$file[[file]] <- list()
    temp_env <- new.env()
    # Source the file into a temporary environment
    source(file, local = temp_env, keep.source = TRUE)
    function_names <- ls(temp_env)

    for (fn_name in function_names) {
      fn <- get(fn_name, envir = temp_env)
      if (is.function(fn)) {
        assign(fn_name, fn, envir = envr)
        function_sources$file[[file]][[fn_name]] <- list(fn = fn_name, fn_source = sprintf("file %s", file))
      }
    }
  }
  return(function_sources$file)
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
assert_all_code_id_sources_exist <- function(x, code_ids){
  from_pkgs <- lapply(x$pkg, names) |> unlist()
  from_source_files <- lapply(x$file, names) |> unlist()
  all_fns <- c(from_pkgs, from_source_files)

  missing_fns <- setdiff(code_ids, all_fns)
  if(length(missing_fns)==0){
    return(invisible(TRUE))
  }
  error_msg <- paste0(
    "The following code_ids were not found in any source:\n",
    paste0("  - '", missing_fns, "'", collapse = "\n"),
    "\n\nPlease ensure these functions are either:\n",
    "  1. Exported from one of the code component packages (like mighty.standards), or\n",
    "  2. Defined in one of the code component source files"
  )

  stop(error_msg)

}
