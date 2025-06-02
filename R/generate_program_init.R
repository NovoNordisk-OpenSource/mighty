#' Write the R code to initialize an ADaM "update" program
#'
#' @param adam_domain
#' @param adam_dataset_list
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_program_init <- function(adam_domain, adam_dataset_list, data_connection, path_out) {
  if(data_connection == "pharmaverse"){
    file_path <- file.path(path_out, paste0(adam_domain, ".R"))

    return(glue::glue("{adam_domain} <- readRDS(\"{file_path}\")"))
  }
  glue::glue(
    "{adam_domain} <- cnt$adam$read_cnt('{adam_domain}')"
  )
}
