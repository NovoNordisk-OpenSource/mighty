#' Assert outputs defined in code components match those defined in yml specs
#'
#' @param x
#'
#' @returns TRUE if passes, error if it does not
#' @export
#'
assert_outputs_identical <- function(x) {
  # Only derivations can have discrepancies as they have "column" defined both
  # in yml and in code components
  x_sub <- x[type_from_code != "row", .(code_id, outputs, outputs_from_code)]
  inx <- purrr::map2(x_sub$outputs, x_sub$outputs_from_code, function(yml, code) {
    setdiff(yml,code) |> length()==0
  }) |> unlist()

  if(sum(!inx)==0) return(TRUE)
  x_error<- x_sub[!inx]
  error_list <- list()
  for(i in seq_len(nrow(x_error))){

    error_list[[x_error[i, code_id]]] <-
      list(Outputs_from_code_component = x_error[i, unlist(outputs_from_code)],
           Outputs_from_specificaton = x_error[i, unlist(outputs)])
  }

  stop(error_list |> pretty_error_outputs())

}
