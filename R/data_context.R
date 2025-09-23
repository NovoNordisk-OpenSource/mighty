#' Data Context Class for Managing Table Metadata
#'
#' @description
#' An R6 class that manages metadata for multiple data tables across different
#' datasources. This class provides a centralized way to access table schemas,
#' variable information, and metadata without loading the actual data.
#'
#' @details
#' The `data_context` class is designed to work with `connector` objects that
#' provide access to different datasources (e.g., SDTM, ADaM). It automatically
#' discovers available tables and their variable structures, storing them as
#' `table_metadata` objects for efficient querying.
#'
#' @section Usage:
#' ```r
#' # Create with connector
#' dc <-  data_context$new(connector = my_connector)
#' 
#' # Create empty and initialize later
#' dc <- data_context$new()
#' dc$init_tables("sdtm")
#' ```
#'
#' @examples
#' \dontrun{
#' # Initialize with connector
#' dc <- data_context$new(connector = my_connector)
#' 
#' # Get available datasources
#' datasources <- dc$get_datasource_names()
#' 
#' # Get table names from SDTM
#' sdtm_tables <- dc$get_table_names("sdtm")
#' 
#' # Check if variable exists
#' has_age <- dc$has_variable("dm", "AGE", "sdtm")
#' 
#' # Get variable information
#' age_type <- dc$get_variable_type("dm", "AGE", "sdtm")
#' age_label <- dc$get_variable_label("dm", "AGE", "sdtm")
#' 
#' # Print summary
#' dc$print_summary()
#' }
#' 
#' # Access tables (read-only)
#' all_tables <- dc$tables
#'
#' @export
data_context <-  R6::R6Class(
  classname = "data_context",
  public = list(
    #' @description
    #' Create a new data_context instance
    #' 
    #' @param connector Optional connector object. If provided, tables will be
    #'   automatically initialized from all available datasources.
    #' 
    #' @return A new `data_context` object
    #' 
    #' @examples
    #' \dontrun{
    #' # Create with connector
    #' dc <- data_context$new(connector = my_connector)
    #' 
    #' # Create empty
    #' dc <- data_context$new()
    #' }
    initialize = function(connector = NULL) {
      dc_initialize(connector, private)
    },

    #' @description
    #' Get names of available datasources
    #' 
    #' @return Character vector of datasource names, or empty vector if no connector
    #' 
    #' @examples
    #' \dontrun{
    #' datasources <- dc$get_datasource_names()
    #' print(datasources)  # e.g., c("sdtm", "adam")
    #' }
    get_datasource_names = function() {
      dc_get_datasource_names(private)
    },
    
    #' @description
    #' Get available table names from specified datasources
    #' 
    #' @param datasources Character vector of datasource names to include.
    #'   Default is "sdtm".
    #' @param prefix_datasource Logical indicating whether to prefix table names
    #'   with datasource name. Default is FALSE.
    #' 
    #' @return Named list where names are "datasource.tablename" and values are
    #'   table names (optionally prefixed with datasource)
    #' 
    #' @examples
    #' \dontrun{
    #' # Get SDTM table names
    #' sdtm_tables <- dc$get_table_names("sdtm")
    #' 
    #' # Get table names with datasource prefix
    #' prefixed_tables <- dc$get_table_names("sdtm", prefix_datasource = TRUE)
    #' 
    #' # Get tables from multiple datasources
    #' all_tables <- dc$get_table_names(c("sdtm", "adam", "metadata"))
    #' }
    get_table_names = function(datasources = c("sdtm"), prefix_datasource = FALSE) {
      dc_get_table_names(datasources, prefix_datasource, private)
    },
    
    #' @description
    #' Get table metadata objects from specified datasources
    #' 
    #' @param datasources Character vector of datasource names to include.
    #'   Default is "sdtm".
    #' 
    #' @return Named list of `table_metadata` objects, keyed by "datasource.tablename"
    #' 
    #' @examples
    #' \dontrun{
    #' # Get SDTM table metadata
    #' sdtm_tables <- dc$get_tables("sdtm")
    #' 
    #' # Access specific table
    #' dm_table <- sdtm_tables[["sdtm.dm"]]
    #' 
    #' # Get tables from multiple datasources
    #' all_tables <- dc$get_tables(c("sdtm", "adam"))
    #' }
    get_tables = function(datasources = c("sdtm")) {
      dc_get_tables(datasources, private)
    },
    
    #' @description
    #' Get variable schema for a specific table
    #' 
    #' @param table_name Character string specifying the table name
    #' @param datasource Character string specifying the datasource name.
    #'   Default is "sdtm".
    #' 
    #' @return Tibble or data.frame containing variable schema (0 rows with column types)
    #' 
    #' @examples
    #' \dontrun{
    #' # Get DM table variables
    #' dm_vars <- dc$get_table_variables("dm", "sdtm")
    #' 
    #' # Inspect variable types
    #' sapply(dm_vars, class)
    #' }
    get_table_variables = function(table_name, datasource = "sdtm") {
      dc_get_table_variables(table_name, datasource, private)
    },
    
    #' @description
    #' Check if a variable exists in a specific table
    #'
    #' @param table_name Character string specifying the table name
    #' @param variable_names Character vector specifying the variable names
    #' @param datasource Character string specifying the datasource name.
    #'   Default is "sdtm".
    #'
    #' @return Logical value indicating whether the variable exists
    #'
    #' @examples
    #' \dontrun{
    #' # Check if AGE exists in DM table
    #' has_age <- dc$has_variables("dm", "AGE", "sdtm")
    #'
    #' # Check multiple variables
    #' variables_exist <- dc$has_variables("dm", c("AGE", "SEX", "RACE"), "sdtm")
    #' }
    has_variables = function(table_name, variable_names, datasource = "sdtm") {
      dc_has_variables(table_name, variable_names, datasource, private)
    },
    
    #' @description
    #' Get the data type of a specific variable
    #' 
    #' @param table_name Character string specifying the table name
    #' @param variable_name Character string specifying the variable name
    #' @param datasource Character string specifying the datasource name.
    #'   Default is "sdtm".
    #' 
    #' @return Character vector of class names, or NULL if variable doesn't exist
    #' 
    #' @examples
    #' \dontrun{
    #' # Get AGE variable type
    #' age_type <- dc$get_variable_type("dm", "AGE", "sdtm")
    #' print(age_type)  # e.g., "numeric"
    #' 
    #' # Check types for multiple variables
    #' var_types <- sapply(c("AGE", "SEX", "USUBJID"), 
    #'                    function(var) dc$get_variable_type("dm", var, "sdtm"))
    #' }
    get_variable_type = function(table_name, variable_name, datasource = "sdtm") {
      dc_get_variable_type(table_name, variable_name, datasource, private)
    },

    #' @description
    #' Get the label attribute of a specific variable
    #' 
    #' @param table_name Character string specifying the table name
    #' @param variable_name Character string specifying the variable name
    #' @param datasource Character string specifying the datasource name.
    #'   Default is "sdtm".
    #' 
    #' @return Character string containing the variable label, or NULL if 
    #'   variable doesn't exist or has no label attribute
    #' 
    #' @examples
    #' \dontrun{
    #' # Get AGE variable label
    #' age_label <- dc$get_variable_label("dm", "AGE", "sdtm")
    #' print(age_label)  # e.g., "Age"
    #' 
    #' # Get labels for multiple variables
    #' var_labels <- sapply(c("AGE", "SEX", "RACE"), 
    #'                     function(var) dc$get_variable_label("dm", var, "sdtm"))
    #' }   
    get_variable_label = function(table_name, variable_name, datasource = "sdtm") {
      dc_get_variable_label(table_name, variable_name, datasource, private)
    },
    
    #' @description
    #' Print a summary of the data context
    #' 
    #' @details
    #' Prints information about the number of tables and their variable counts.
    #' Useful for getting an overview of the available data structure.
    #' 
    #' @examples
    #' \dontrun{
    #' # Print summary
    #' dc$print()
    #' }
    print = function() {
      dc_print(self, private)
    }
  ),
  active = list(
    #' @field tables List of `table_metadata` objects, keyed by "datasource.tablename"
    tables = \() private$.tables
  ),
  private = list(
    .tables = NULL,
    .cnt = NULL,
    .datasources = NULL

  )
)

#' @noRd
dc_initialize <- function(connector, private) {
  private$.tables <- list()
  private$.cnt <- connector
  if (!is.null(connector)) {
    dc_init_tables(NULL, private)
  }
}

#' @noRd
dc_init_tables <- function(datasourcename, private) {
  if (!is.null(datasourcename)) {
    private$.datasources <- c(datasourcename)
  }
  else {
    private$.datasources <- dc_get_datasource_names(private)
  }
  for (ds in private$.datasources) {
    zephyr::msg_debug(paste0("Adding tables from ", ds))
    if (!is.null(private$.cnt[[ds]])) {
      for (tbl in (private$.cnt[ds])) {
        datasets <- tbl$list_content_cnt()
        if (length(datasets) == 0) {
          zephyr::msg_debug(paste0("No data in ", ds))
        }
        else {
          for (domain in datasets) {
            domain_name <- sub("\\.[^.]+$", "", domain)
            variables <- tbl$tbl_cnt(domain) |>
              head(0) |>
              dplyr::collect()
            table <- table_metadata$new(name=domain_name, datasource=ds, variables=variables)
            private$.tables[[paste0(ds, ".", domain_name)]] <- table
            zephyr::msg_debug(paste0("Adding ", ds, ".", domain_name, " from ", domain))
          }
        }
      }
    }
    else {
      zephyr::msg_debug(paste0("No datasource '", ds, "'."))
    }
  }
}

#' @noRd
dc_get_datasource_names <- function(private){
  if (is.null(private$.cnt)) {
    return(character(0))
  }
  return(names(private$.cnt))
}

#' @noRd
dc_get_table_names <- function(datasources = c("sdtm"), prefix_datasource = FALSE, private){
  table_names <- list()
  for (table in private$.tables) {
    if (table$datasource %in% datasources) {
      table_names[paste0(table$datasource, ".", table$name)] <- 
        paste0(ifelse(prefix_datasource, paste0(table$datasource, "."), ""), 
      table$name)
    }
  }
  table_names
}

#' @noRd
dc_get_tables <- function(datasources = c("sdtm"), private) {
  tables <- list()
  for (table in private$.tables) {
    if (table$datasource %in% datasources) {
      tables[[paste0(table$datasource, ".", table$name)]] <- table
    }
  }
  tables
}

#' @noRd
dc_get_table_variables <- function(table_name, datasource = "sdtm", private){
  return(dc_get_tables(c(datasource), private)[[paste0(datasource, ".", table_name)]]$variables)
}

#' @noRd
dc_has_variables <- function(table_name, variable_names, datasource = "sdtm", private) {
  variables <- dc_get_table_variables(table_name, datasource, private)
  return(all(variable_names %in% names(variables)))
}
 
#' @noRd
dc_get_variable_type <- function(table_name, variable_name, datasource = "sdtm", private){
  if (dc_has_variables(table_name, variable_name, datasource, private)) {
    variable <- dc_get_table_variables(table_name, datasource, private)[variable_name]
    return(class(variable[[1]]))
  }
  return(NULL)
}

#' @noRd
dc_get_variable_label <- function(table_name, variable_name, datasource = "sdtm", private)
{  if (dc_has_variables(table_name, variable_name, datasource, private)) {
    variable <- dc_get_table_variables(table_name, datasource, private)[variable_name]
    return(attr(variable[[1]], "label"))
  }
  return(NULL)
}

#' @noRd
dc_print <- function(self, private) {
  cat("data_context Summary:\n")
  cat("=====================\n")
  cat("Tables:", length(private$.tables), "\n")
  
  for (table_name in names(private$.tables)) {
    table_meta <- private$.tables[[table_name]]
    cat(sprintf("Table: %s (%d variables)\n", 
                table_name, ncol(table_meta$variables)))
  }
}
