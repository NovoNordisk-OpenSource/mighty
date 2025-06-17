#' Write the R code to initialize an ADaM "update" program
#'
#' @param adam_domain
#' @param adam_dataset_list
#'
#' @return
#' @export
#'
#' @examples
generate_program_init <- function(adam_domain, adam_dataset_list, path_out) {
  glue::glue(
    "{adam_domain} <- cnt$adam$read_cnt('{adam_domain}')"
  )
}
