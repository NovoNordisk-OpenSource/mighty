#' Utility function to replace default parameter values with user-supplied
#' values
#'
#' @param call_obj
#' @param user_params
#'
#' @returns
#' @export
#'
#' @examples
update_default_parameters <- function(call_obj, user_params) {
  if (!inherits(call_obj, "call")) {
    stop("First argument must be a call object")
  }

  # Remove the first element as this should always be "list"
  call_names <- names(as.list(call_obj)[-1])

  # Iterate through user parameters
  for (param_name in names(user_params)) {
    param_exists_in_fn <- param_name %in% call_names
    if (!param_exists_in_fn) {
      next
    }
    param_value <- user_params[[param_name]]

    # This logic is needed to avoid trying to parse strings as R code
    is_string <- is.character(param_value) &&
      grepl("^s:", param_value)
    if (is_string) {
      # If not R code, return literal value
      string_ <- sub("^s:", "", param_value)
      call_obj[[param_name]] <- string_
      next
    }

    # When param IS R code, need to try and parse it
    tryCatch({
      new_expr <- parse(text = param_value)[[1]]
      call_obj[[param_name]] <- new_expr
    }, error = function(e) {

      warning("Failed to parse R code: ", r_code, " - using as literal string")
      call_obj[[param_name]] <- r_code
    })

  }

  return(call_obj)
}
