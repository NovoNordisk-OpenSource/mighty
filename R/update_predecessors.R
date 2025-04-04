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
      if (domain_i != dep_domain) {

        # Update depend_cols
        new_dep_cols <- lapply(pk[[dep_domain]], function(x) {
          data.table(
            column_name = c(x, x),
            domain = c(domain_i, dep_domain),
            domain_type = c("adam", classify_external_data_domains_2(dep_domain)),
            full_name =  paste(c(domain_i, dep_domain), x, sep = ".")
          )
        }) |> rbindlist()
        x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]],
                                                  new_dep_cols)
      }
    }
  }
  return(x)
}
