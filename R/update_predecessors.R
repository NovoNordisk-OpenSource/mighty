#' @title Update predecessors
#' @description
#' Ensures that external domains
#'
#' Additional details...
#'
#' @param nodes
#' @param path_mappings
#'
#' @return
#' @export
#'
#' @examples
update_predecessors <- function(nodes, path_mappings) {

  x <- copy(nodes)

  pk <- yaml::read_yaml(path_mappings)

  for (i in 1:nrow(x)) {
    # If node is a predecessor, i.e. have missing code_id
    if (is.na(x[["code_id"]][[i]])) {

      domain_i <- x[["domain"]][[i]]

      # Impute missing values for predecessor
      x[i, type := "predecessor"]

      # Domain of dependent column
      dep_domain <- x[["depend_cols"]][[i]][["domain"]]

      # If the dependent column originates from an external domain, then add
      # addition dependent columns
      # if (domain_i != dep_domain) {
      #
      #   # Update depend_cols
      #   new_dep_cols <- lapply(pk[[dep_domain]], function(x) {
      #     data.table(
      #       column_name = c(x, x),
      #       domain = c(domain_i, dep_domain),
      #       domain_type = c("adam", classify_external_data_domains(dep_domain))
      #     )
      #   }) |> rbindlist()
      #   x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]],
      #                                             new_dep_cols)
      # }
    }
  }
  return(x)
}
