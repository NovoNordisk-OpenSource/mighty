#' Read ADaM Domain Specifications from YAML Files
#'
#' @description
#' Reads and processes ADaM domain specifications from one or more YAML files,
#' converting them into a standardized internal format for code generation.
#'
#' @details
#' This function serves as the primary entry point for loading ADaM domain
#' specifications from YAML configuration files. It processes each file through
#' \code{\link{read_adam_domain_yml}} and combines the results into a unified
#' structure.
#'
#' Each YAML file should contain:
#' - **table_metadata**: Domain name, keys, and other table-level information
#' - **column_metadata**: Column definitions with dependencies, outputs, and types
#' - **row_actions**: Optional row-level operations and transformations
#' - **init**: Domain initialization settings including core domains and filters
#'
#' @param paths Character vector of file paths to YAML files containing ADaM
#'   domain specifications. Each file should contain a complete domain definition
#'   following the expected YAML schema.
#'
#' @return A named list where each element represents an ADaM domain specification.
#'   Each domain element contains:
#'   \item{columns}{List of column definitions with dependencies, outputs, types,
#'     and code references}
#'   \item{domain}{Character string specifying the domain name (e.g., "ADSL", "ADLB")}
#'   \item{keys}{Character vector of primary key columns for the domain}
#'   \item{init}{List containing initialization settings including core_domains,
#'     filter specifications,
read_adam_specs <- function(paths){
  missing_files <- paths[!file.exists(paths)]
  if (length(missing_files) > 0) {
    stop("The following ADaM Specification file(s) do not exist: ",
         paste0("'", missing_files, "'", collapse = ", "))
  }


  out <- lapply(paths, read_adam_domain_yml) |>
    unlist(recursive = FALSE)

  assert_valid_ui_yml(out)
  return(out)
}


read_adam_domain_yml <- function(yml) {
  if(!file.exists(yml)){
    stop("ADaM Specification file '", yml, "' does not exist.")
  }

  x <- yaml::read_yaml(yml)
  # Name elements in the list
  names(x$column_metadata) <- lapply(x$column_metadata, function(i){i$column})
  if(!is.null(x$row_actions)){

    # names(x$row_actions) <- lapply(x$row_actions, function(i){i$code_id})
    tmp <- c(x$column_metadata, x$row_actions)
  }
  else{
    tmp <- x$column_metadata
  }
  # Restructure to match internal data model
  out <- lapply(tmp, function(i) {
    # rename the element "source" to "depend_col"
    i$depend_cols <- i$source
    i$outputs <- i$column
    i$source <- i$column <- NULL

    # if elements don't exist, add them with a value of NA
    names_i <- names(i)
    if (!"depend_rows" %in% names_i) {
      i$depend_rows <- NA
    }
    if (!"parameters" %in% names_i) {
      i$parameters <- NA
    }
    return(i)
  })

  return_list <- list(columns = out,
       domain = x$table_metadata$table,
       keys = x$table_metadata$keys,
       init = x$init) |>
  convert_to_NA_character()
  return(setNames(list(return_list), return_list$domain))


}

convert_to_NA_character <- function(x) {
  # Check if x is a list
  if (is.list(x)) {
    # Apply recursively
    return(lapply(x, convert_to_NA_character))
  }


  if (is.character(x) && any(x == "NA")) {
    x[x == "NA"] <- NA_character_
  }

  # Return the possibly modified x
  return(x)
}
