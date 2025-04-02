#' make_adam_program
#'
#' @param path_ui_data
#' @param path_std_lib
#' @param path_domain_keys
#' @param path_output
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
make_adam_program <- function(path_ui_data,
                              path_std_lib,
                              path_domain_keys,
                              path_output,
                              data_connection = c("connector", "pharmaverse")) {
  session_output <- generate_adam_code(path_ui_data,
                                       path_std_lib,
                                       path_domain_keys,
                                       path_output,
                                       data_connection)
  write_adam_programs(session_output$programs, path_output)
  return(
    list(
      program_order = session_output$program_order,
      edges = session_output$edges,
      program_order_complete = session_output$program_order_3,
      data_model = session_output$ui_data_2$nodes
    )
  )
}
