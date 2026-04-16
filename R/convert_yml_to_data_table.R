#' Convert YAML list to data.table format
#'
#' This function converts a YAML list structure containing column definitions
#' into a data.table format. It processes multiple domains and their associated
#' column configurations.
#'
#' @param yml_list A list containing YAML data with column definitions.
#'   Each element should have a `columns` component (nested list of column
#'   definitions) and a `domain` component (character string identifying
#'   the domain).
#'
#' @return A data.table with columns:
#'   \describe{
#'     \item{column}{Character. Column names from the YAML}
#'     \item{type}{Character. Data type of each column}
#'     \item{depend_cols}{List. Columns that this column depends on}
#'     \item{outputs}{List. Output specifications for the column}
#'     \item{depend_rows}{List. Row dependencies for the column}
#'     \item{parameters}{List. Parameters associated with the column}
#'     \item{code_id}{Character. Code identifier for the column}
#'     \item{domain}{Character. Domain name from the YAML}
#'     \item{id}{Character. Unique identifier for the column}
#'   }
#'
#' @examples
#' \dontrun{
#' # Example YAML structure
#' yml_data <-  list(
#'   list(
#'     domain = "example_domain",
#'     columns = list(
#'       col1 = list(type = "numeric", id = "1"),
#'       col2 = list(type = "character", id = "2")
#'     )
#'   )
#' )
#' result <- convert_yml_to_data_table(yml_data)
#' }
#'
#' @seealso [convert_yml_to_data_table_()] for the internal conversion function
#' @noRd
convert_yml_to_data_table <- function(yml_list) {
  lapply(yml_list, function(i) {
    convert_yml_to_data_table_(i$columns, i$domain)
  }) |>
    data.table::rbindlist()
}

#' Internal function to convert nested YAML list to data.table
#'
#' This is an internal helper function that processes a single domain's
#' column definitions from YAML format into a structured data.table.
#'
#' @param nested_list A nested list containing column definitions where
#'   names are column names and values are lists of column properties
#'   (type, depend_cols, outputs, parameters, etc.).
#' @param domain Character string identifying the domain name.
#'
#' @return A data.table with one row per column containing all the
#'   extracted properties. See [convert_yml_to_data_table()] for
#'   column descriptions.
#'
#' @details
#' This function extracts the following properties from each column definition:
#' \itemize{
#'   \item type: Data type specification
#'   \item depend_cols: Column dependencies
#'   \item outputs: Output configurations
#'   \item parameters: Associated parameters
#'   \item depend_rows: Row dependencies
#'   \item code_id: Code identifier
#'   \item id: Unique identifier
#' }
#'
#' Missing values are handled by setting them to `NA_character_` or
#' `list(NA_character_)` as appropriate.
#'
#' @keywords internal
#' @noRd
convert_yml_to_data_table_ <- function(nested_list, domain) {
  parent_names <- names(nested_list)

  # Initialize empty lists to store each column
  type_list <- vector("character", length(parent_names))
  depend_cols_list <- vector("list", length(parent_names))
  outputs_list <- vector("list", length(parent_names))
  code_id_chr <- vector("character", length(parent_names))
  parameters <- vector("character", length(parent_names))
  depend_rows_list <- vector("character", length(parent_names))
  parent_names_chr <- vector("character", length(parent_names))
  id_chr <- vector("character", length(parent_names))

  # Extract data from each parent element
  for (i in seq_along(parent_names)) {
    parent_data <- nested_list[[i]]
    parent_names_chr[[i]] <- list(parent_names[i])
    type_list[[i]] <- if (!is.null(parent_data$type)) {
      parent_data$type
    } else {
      NA_character_
    }
    depend_cols_list[[i]] <- if (!is.null(parent_data$depend_cols)) {
      list(parent_data$depend_cols)
    } else {
      list(NA_character_)
    }
    parameters[[i]] <- if (all(!is.na(parent_data$parameters))) {
      list(unlist(parent_data$parameters, recursive = FALSE))
    } else {
      list(NA_character_)
    }
    outputs_list[[i]] <- if (!is.null(parent_data$outputs)) {
      list(parent_data$outputs)
    } else {
      list(NA_character_)
    }
    depend_rows_list[[i]] <- if (!is.null(parent_data$depend_rows)) {
      list(parent_data$depend_rows)
    } else {
      list(NA_character_)
    }
    code_id_chr[[i]] <- if (!is.null(parent_data$code_id)) {
      parent_data$code_id
    } else {
      NA_character_
    }
    id_chr[[i]] <- if (!is.null(parent_data$id)) {
      parent_data$id
    } else {
      NA_character_
    }
  }

  dt <- data.table::data.table(
    column = unlist(parent_names_chr, recursive = FALSE),
    type = type_list,
    depend_cols = unlist(depend_cols_list, recursive = FALSE),
    outputs = unlist(outputs_list, recursive = FALSE),
    depend_rows = unlist(depend_rows_list, recursive = FALSE),
    parameters = unlist(parameters, recursive = FALSE),
    code_id = code_id_chr,
    domain = rep(domain, length(parent_names)),
    id = unlist(id_chr, recursive = FALSE)
  )

  return(dt)
}
