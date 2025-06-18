#' Assert outputs defined in code components match those defined in yml specs
#' @description Validates that the outputs defined in code components match
#' exactly with those defined in the YAML specifications.
#'
#' @details This function checks for consistency between the outputs declared in
#' YAML specifications and those actually produced by the code components. It
#' focuses specifically on 'col_compute' type components since these have
#' outputs defined in both places. The function compares the two sets of outputs
#' and raises a detailed error if any discrepancies are found, showing exactly
#' which outputs differ for each code component.
#'
#' @param x A data.table containing code components with columns:
#'   - type_from_code: The type of the component
#'   - code_id: Unique identifier for the code component
#'   - outputs: List column containing outputs defined in YAML specs
#'   - outputs_from_code: List column containing outputs detected from code
#'
#' @return TRUE if all outputs match, or throws an error with detailed
#'   information about mismatches
assert_outputs_identical <- function(x) {
  # Only col_compute can have discrepancies as they have "column" defined both
  # in yml and in code components

  x_sub <- x[type_from_code %in% c("col_compute", "col_supp"), .(code_id, outputs, outputs_from_code)]
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
