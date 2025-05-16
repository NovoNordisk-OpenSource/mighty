#' @title Update predecessors
#' @description
#' Ensures that external domains
#'
#' Additional details...
#'
#' @param nodes
#' @param pk
#' @param ui_init
#'
#' @return
#' @export
#'
#' @examples
update_predecessors <-  function(nodes, pk, ui_init) {

  x <- copy(nodes)

  # Extract domain of dependency columns
  dep_domain <- unlist(lapply(x$depend_cols, function(i) {
    if (nrow(i) > 1) {
      # Ignore if multiple dependencies are present in which case the action is
      # not a predecessor and needs no update
      return("")
    }
    i$domain
  }))

  # Identify indices of predecessors:
  predecessor_indices <- which(is.na(x[["code_id"]]) &
                                 (dep_domain == "core" |
                                    dep_domain != x[["domain"]]))

  # If no predecessors are found, return early
  if (length(predecessor_indices) == 0) {
    return(x)
  }

  # Assign type to "predecessor" for all actions that are predecessors
  x[predecessor_indices, type := "predecessor"]

  # Identify predecessors that are
  #   1. external (from different domains than core) and
  #   2. core
  dep_domains <- vapply(x[["depend_cols"]][predecessor_indices], function(dc) dc[["domain"]], character(1))
  predecessor_ext <- x[["domain"]][predecessor_indices] != dep_domains & dep_domains != "core"
  predecessor_core <- dep_domains == "core"

  # 1. Update external predecessors column dependency with foreign key
  for (i in predecessor_indices[predecessor_ext]) {
    domain_i <- x[["domain"]][[i]]
    dep_domain <- x[["depend_cols"]][[i]][["domain"]]

    if(is.null(pk[[toupper(dep_domain)]])){
      stop(paste0("Domain '", dep_domain, "' not recognised for foreign key lookup."))
    }

    new_dep_cols <- lapply(pk[[toupper(dep_domain)]], function(col) {
      data.table(
        column_name = c(col, col),
        domain = c(domain_i, dep_domain),
        domain_type = c("adam", classify_external_data_domains(dep_domain))
      )
    }) |> rbindlist()

    x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]], new_dep_cols)
  }


  # 2. Update core predecessors
  for (i in predecessor_indices[predecessor_core]) {

    dep_cols_i <- x[["depend_cols"]][[i]]
    domain_i <- x[["domain"]][[i]]
    core_domains <- ui_init[[domain_i]][["core_domains"]]

    # Replace core domain with actual domain(s)s
    new_dep_cols <- data.table(
      column_name = dep_cols_i[["column_name"]],
      domain = core_domains,
      domain_type = classify_external_data_domains(core_domains)
    )
    x[["depend_cols"]][[i]] <- new_dep_cols
  }

  return(x)
}
