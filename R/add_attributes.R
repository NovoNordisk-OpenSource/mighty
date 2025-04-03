#' @title Add attributes to a list of nodes
#'
#' @param input_list
#'
#' @return
#'
#' @examples
add_attributes <- function(input_list) {

  fields <- c("depend_cols_complete",
              "outputs",
              "outputs_complete")

  out <- lapply(input_list, function(node_i) {
    for(field in fields){
      # get everything after the "." in the field name
      field_i <- node_i[[field]]

      if(is.null(field_i)|| any(is.na(field_i))){
        field_i <- character(0L)
      }
      split_field <- strsplit(field_i, "\\.")
      column_name <- vapply(split_field, function(x) x[2], character(1L))
      domain <- vapply(split_field, function(x) x[1], character(1L))

      out <- data.table::data.table(
        column_name=column_name,
        domain=domain,
        domain_type = classify_external_data_domains_2(domain),
        full_name = field_i
      )

      node_i[[field]] <- out

    }

    return(node_i)
  })
}
