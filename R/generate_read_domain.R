#' Write the R code to read ADaM domain in "update" program
#'
#' @param adam_domain
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_read_domain <- function(adam_domain, data_connection, path_out) {
  read_domain_code <- if (data_connection == "pharmaverse") {
    file_path <- file.path(path_out, paste0(adam_domain, ".R"))
    glue::glue("{adam_domain} <- readRDS(\"{file_path}\")")
  } else {
    glue::glue("{adam_domain} <- cnt$adam$read_cnt('{adam_domain}')")
  }
  return(paste0(read_domain_code, "\n"))
}
