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
update_predecessors <-  function(nodes, path_mappings) {

  x <- copy(nodes)

  pk <- yaml::read_yaml(path_mappings)

  # Vectorize NA check for code_id and domain mismatch
  na_code_id_indices <- which(is.na(x[["code_id"]]))

  if (length(na_code_id_indices) == 0) {
    return(x)  # No NA code_id, early exit
  }

  x[na_code_id_indices, type := "predecessor"]

  dep_domains <- vapply(x[["depend_cols"]][na_code_id_indices], function(dc) dc[["domain"]], character(1))
  domain_mismatches <- x[["domain"]][na_code_id_indices] != dep_domains & dep_domains != "core"

  # Process only mismatched domains
  mismatched_indices <- na_code_id_indices[domain_mismatches]

  if (length(mismatched_indices) == 0) {
    return(x)  # No domain mismatches, early exit after setting type
  }

  for (i in mismatched_indices) {
    domain_i <- x[["domain"]][[i]]
    dep_domain <- x[["depend_cols"]][[i]][["domain"]]

    # Condition already ensured by domain_mismatches
    new_dep_cols <- lapply(pk[[dep_domain]], function(col) {
      data.table(
        column_name = c(col, col),
        domain = c(domain_i, dep_domain),
        domain_type = c("adam", classify_external_data_domains(dep_domain))
      )
    }) |> rbindlist()

    x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]], new_dep_cols)
  }

  return(x)
}
