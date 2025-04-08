#' Flatten top two level
#'
#' @description Used when re-shaping the metadata model so that it can be
#' efficently processed to make the dependency graph. Without this, it would
#' require much more traversing of the list structure
#'
#'
#' @param input_list
#'
#' @return
#' @export
#'
#' @examples
flatten_top_two_levels <- function(input_list) {
  result <- list()

  for (domain_name in names(input_list)) {
    for (action_name in names(input_list[[domain_name]])) {

      new_key <- paste(domain_name, action_name, sep = ".")
      result[[new_key]] <- input_list[[domain_name]][[action_name]]
      result[[new_key]][["domain"]] <- domain_name
    }
  }

  return(result)
}
