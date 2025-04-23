#' Write the R code to initialize an ADaM "update" program
#'
#' @param adam_domain
#' @param adam_dataset_list
#' @param data_connection
#' @param file_extension
#'
#' @return
#' @export
#'
#' @examples
generate_program_init <- function(adam_domain, adam_dataset_list, data_connection, path_out, file_extension = c("parquet", "sas7bdat")){
  if(data_connection == "pharmaverse"){
    file_path <- file.path(path_out, paste0(adam_domain, ".R"))

    return(glue::glue("{adam_domain} <- readRDS(\"{file_path}\")"))
  }
  adam_domain_ext <- make_adam_domain_ext(adam_domain, file_extension, adam_dataset_list)
  glue::glue(
    "{adam_domain} <- adam_connector |>
    connector::cnt_read('{adam_domain_ext}')"
  )
}
