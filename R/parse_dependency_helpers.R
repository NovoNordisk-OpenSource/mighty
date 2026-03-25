#' Check if a dependency string references a row or parameter action
#'
#' @param dep_string Character vector of dependency strings
#' @return Logical vector indicating if each string is a row/parameter reference
#' @noRd
is_row_dependency <- function(dep_string) {
  grepl("^(rows|parameters)\\.", dep_string)
}


#' Check if a dependency string is domain-qualified
#'
#' @param dep_string Character vector of dependency strings
#' @return Logical vector indicating if each string has a domain prefix
#' @noRd
has_domain_prefix <- function(dep_string) {
  has_dot <- grepl("\\.", dep_string)
  has_dot & !is_row_dependency(dep_string)
}


#' Extract domain prefix from dependency strings
#'
#' Only works on domain-prefixed strings (e.g., "adsl.AGE"). Use after
#' filtering with `has_domain_prefix()`.
#'
#' @param dep_string Character vector of domain-qualified dependency strings
#' @return Character vector of domain names
#' @noRd
extract_domain_prefix <- function(dep_string) {
  dep_string |> has_domain_prefix() |> all() |> checkmate::assert_true()
  sub("\\..*", "", dep_string)
}


#' Extract the identifier from a dependency string
#'
#' @param dep_string Character vector of dependency strings
#' @return Character vector of identifiers (column names or action ids)
#' @noRd
extract_dependency_id <- function(dep_string) {
  has_dot <- grepl("\\.", dep_string)
  result <- dep_string
  result[has_dot] <- sub("^[^.]+\\.", "", dep_string[has_dot])
  result
}


#' Create Qualified Column References
#'
#' Constructs domain-qualified column references in the format "domain.column_name"
#' from a data structure containing domain and column_name columns.
#'
#' @param df Data frame or data.table with 'domain' and 'column_name' columns
#' @return Character vector of qualified references (e.g., "ADSL.AGE", "dm.USUBJID")
#' @noRd
#'
#' @examples
#' \dontrun{
#' deps <- data.table(domain = c("ADSL", "dm"), column_name = c("AGE", "USUBJID"))
#' qualify_column_refs(deps)  # Returns: c("ADSL.AGE", "dm.USUBJID")
#' }
qualify_column_refs <- function(df) {
  if (nrow(df) == 0) {
    return(character(0))
  }
  paste0(df$domain, ".", df$column_name)
}
