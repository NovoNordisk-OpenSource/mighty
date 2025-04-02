#' @title Update predecessors
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
      action_i <- x[["action"]][[i]]

      # Impute missing values for predecessor
      x[i, type := "column"]
      x[i, origin := "predecessor"]

      # Domain of dependent column
      dep_domain <- x[["depend_cols"]][[i]][["domain"]]

      # If the dependent column originates from an external domain, then add
      # addition dependent columns
      if (dep_domain != "self") {

        # Update depend_cols
        new_dep_cols <- lapply(pk[[dep_domain]], function(x) {
          data.table(
            column_name = c(x, x),
            domain = c("self", dep_domain),
            domain_type = c("adam", classify_external_data_domains_2(dep_domain)),
            full_name =  paste(c("self", dep_domain), x, sep = ".")
          )
        }) |> rbindlist()
        x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]],
                                         new_dep_cols)

        # Update depend_cols_complete
        new_dep_cols_complete <- lapply(pk[[dep_domain]], function(x) {
          data.table(
            column_name = c(x, x),
            domain = c(domain_i, dep_domain),
            domain_type = c("adam", classify_external_data_domains_2(dep_domain)),
            full_name =  paste(c(domain_i, dep_domain), x, sep = ".")
          )
        }) |> rbindlist()
        x[["depend_cols_complete"]][[i]] <- rbind(x[["depend_cols_complete"]][[i]],
                                                  new_dep_cols_complete)
      }
    }
  }
  return(x)
}
