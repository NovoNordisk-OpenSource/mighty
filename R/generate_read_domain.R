#' Write the R code to read ADaM domain in "update" program
#'
#' @param adam_domain
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_read_domain <- function(adam_domain) {
  glue::glue(
    "{adam_domain} <- cnt$adam$read_cnt('{adam_domain}')"
  )
}
