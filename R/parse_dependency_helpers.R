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
