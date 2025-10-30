#' Table Metadata Class
#'
#' @description
#' Stores metadata information for a single table including variable names and types.
#'
#' @noRd
table_metadata <- R6::R6Class(
  classname = "table_metadata",
  public = list(
    #' @description
    #' Create a new table_metadata object
    #'
    #' @param name Character. Table name
    #' @param datasource Character. Model type (e.g. SDTM, ADAM, or METADATA)
    #' @param variables Data.frame. Variable metadata
    initialize = function(name, datasource, variables) {
      tm_initialize(name, datasource, variables, private)
    },

    #' @description
    #' Print table metadata
    print = function() {
      tm_print(private)
    }
  ),

  active = list(
    #' @field variables Data.frame. Variables with name and type columns
    variables = \() private$.variables,
    #' @field datasource Character. Model type, i.e. `sdtm`, `adam` or `metadata`
    datasource = \() private$.datasource,
    #' @field name Character. Name of the table
    name = \() private$.name
  ),

  private = list(
    .name = NULL,
    .datasource = NULL,
    .variables = NULL
  )
)

tm_initialize <- function(name, datasource, variables, private) {
  private$.name <- name
  private$.datasource <- datasource
  private$.variables <- variables
}

tm_print <- function(private) {
  cat(sprintf("Table: %s\n", private$.name))
  cat(sprintf("Data model:  %s\n", private$.datasource))
  cat("Variables:\n")
  print(private$.variables)
}
