#' Merge yaml files
#'
#' @param yaml_paths
#'
#' @return
#' @export
#'
#' @examples
merge_yaml_files <- function(yaml_paths) {
  yaml_list <- lapply(yaml_paths, yaml::read_yaml)
  result <- list()
  # Merge each YAML file into the result
  for (yaml_data in yaml_list) {
    result <- merge_yaml_lists(result, yaml_data)
  }

  return(result)
}


merge_yaml_lists <- function(list1, list2) {
  if (is.null(list1)) return(list2)
  if (is.null(list2)) return(list1)

  # Get all unique keys from both lists
  all_keys <- unique(c(names(list1), names(list2)))

  # Create a new list with merged content
  result <- list()

  for (key in all_keys) {
    # If key exists only in one list, use that value
    if (!(key %in% names(list1))) {
      result[[key]] <- list2[[key]]
    } else if (!(key %in% names(list2))) {
      result[[key]] <- list1[[key]]
    } else {
      # If key exists in both lists
      val1 <- list1[[key]]
      val2 <- list2[[key]]

      # If both values are lists with names, merge them recursively
      if (is.list(val1) && is.list(val2) && !is.null(names(val1)) && !is.null(names(val2))) {
        result[[key]] <- merge_yaml_lists(val1, val2)
      } else {
        # For other cases (including named lists and vectors), keep both values
        # For vectors, combine them uniquely if they're the same type
        if (is.atomic(val1) && is.atomic(val2) && typeof(val1) == typeof(val2)) {
          result[[key]] <- unique(c(val1, val2))
        } else if (is.list(val1) && is.list(val2) && is.null(names(val1)) && is.null(names(val2))) {
          # For unnamed lists (like arrays in YAML), combine uniquely
          result[[key]] <- unique(c(val1, val2))
        } else {
          # If types don't match or we can't combine, use the second value
          # You might want to customize this behavior
          result[[key]] <- val2
        }
      }
    }
  }

  return(result)
}
