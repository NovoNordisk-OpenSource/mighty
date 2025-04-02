#' Title
#'
#' @param func
#'
#' @return
#' @export
#'
#' @examples
get_scope_objects <- function(func) {
  body_expr <- body(func)

  # Function to recursively extract assignments
  extract_assignments <- function(expr, exclude_inner_funcs = TRUE) {
    if (!is.call(expr)) return(character())

    # If we encounter a function definition and exclude_inner_funcs is TRUE,
    # skip its body
    if (exclude_inner_funcs && identical(expr[[1]], as.name("function"))) {
      return(character())
    }

    # If we encounter an assignment, return the name being assigned to
    if (as.character(expr[[1]]) %in% c("<-", "=")) {
      if (is.name(expr[[2]])) {
        return(as.character(expr[[2]]))
      }
    }
    # Recursively process all parts of the expression
    unique(unlist(lapply(expr, extract_assignments, exclude_inner_funcs)))
  }
  assignments <- extract_assignments(body_expr)
  # Remove function arguments from the result.
  # TODO: MEWP - not sure if this is needed
  func_args <- names(formals(func))
  assignments <- setdiff(assignments, func_args)
  return(assignments)
}
