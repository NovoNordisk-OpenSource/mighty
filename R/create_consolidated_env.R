#' Create consolidate environment for all code components
#'
#' @param packages
#' @param source_files
#'
#' @returns Returns an environment
#' @export
#'
#' @examples
create_consolidated_env <- function(packages = NULL, source_files = NULL, code_ids) {
  # Create a new environment
  consolidated_env <- new.env(parent = emptyenv())

  # Load functions from packages
  if (!is.null(packages)) {
    for (pkg in packages) {
      load_fn_to_env_pkg(pkg, consolidated_env, code_ids)
    }
  }

  # Source files and load their functions
  if (!is.null(source_files)) {
    for (file in source_files) {
      load_fn_to_env_source_file(file, consolidated_env)
    }
  }

  return(consolidated_env)
}


load_fn_to_env_pkg <- function(pkg, target_env, code_ids) {
  # Check if package is installed
  if (!requireNamespace(pkg, quietly = TRUE)) {
    warning(paste("Package", pkg, "is not available and will be skipped."))
    return(invisible(FALSE))
  }

  # Access the package namespace directly without loading it into search path
  pkg_ns <- asNamespace(pkg)

  # Get exported objects from the package
  pkg_exports <- getNamespaceExports(pkg)

  code_ids_in_pkg <- intersect(code_ids, pkg_exports)
  # Copy only functions to our target environment
  for (obj_name in code_ids_in_pkg) {
    obj <- get(obj_name, envir = pkg_ns)
    if (is.function(obj)) {
      assign(obj_name, obj, envir = target_env)
    }
  }

  return(invisible(TRUE))
}

# Function to load functions from a single source file
load_fn_to_env_source_file <- function(file, target_env) {
  if (!file.exists(file)) {
    warning(paste("File", file, "does not exist and will be skipped."))
    return(invisible(FALSE))
  }

  # Create a temporary environment to source the file
  temp_env <- new.env()
  source(file, local = temp_env, keep.source = TRUE)

  # Copy functions to our target environment
  for (obj_name in ls(temp_env)) {
    obj <- get(obj_name, envir = temp_env)
    if (is.function(obj)) {
      assign(obj_name, obj, envir = target_env)
    }
  }

  return(invisible(TRUE))
}

