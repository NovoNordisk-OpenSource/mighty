#' Normalize YAML Structure for Consistent Comparison
#'
#' @description
#' Recursively normalizes a YAML data structure by sorting named list elements
#' alphabetically. This function ensures consistent ordering of object keys
#' throughout nested structures, making it suitable for semantic comparison
#' of YAML data where key ordering differences should be ignored.
#'
#' @param yaml_obj A list object, typically created by parsing YAML content
#'   with \code{yaml::yaml.load()}. Can contain nested lists, vectors, and
#'   other R data types.
#'
#' @return A list with the same structure and content as the input, but with
#'   all named list elements sorted alphabetically by their names. Unnamed
#'   lists (arrays) and non-list elements are returned unchanged.
#'
#' @details
#' The function performs the following operations:
#' \itemize{
#'   \item Recursively processes all nested list structures
#'   \item Sorts named lists (objects) by their key names alphabetically
#'   \item Preserves the order of unnamed lists (arrays)
#'   \item Leaves non-list elements (scalars, vectors) unchanged
#' }
#'
#' This normalization is particularly useful when comparing YAML structures
#' where the semantic content is identical but key ordering differs, such as
#' when comparing configuration files or data structures from different sources.
#'
#' @note
#' \strong{Limitations:}
#' \itemize{
#'   \item Does not sort array elements - only object keys
#'   \item Array order is preserved, which may cause semantic equivalents
#'     to appear different if array element order varies
#'   \item For complete semantic comparison, consider additional normalization
#'     of array contents if order independence is required
#' }
#'
normalize_yaml_structure <- function(yaml_obj) {
  if (is.list(yaml_obj)) {
    # Recursively normalize all list elements
    yaml_obj <- lapply(yaml_obj, normalize_yaml_structure)

    # Sort named lists by names to ensure consistent ordering
    if (!is.null(names(yaml_obj))) {
      yaml_obj <- yaml_obj[sort(names(yaml_obj))]
    }
  }
  return(yaml_obj)
}
