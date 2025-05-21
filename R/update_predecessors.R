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
  dep_domain <- extract_domain_of_dependency_columns(x)

  # Identify indices of of copy nodes and rename nodes
  index_copy_mutate <- which(is.na(x[["code_id"]]) &
                               dep_domain == "core")

  index_echos <- x[, code_id] |>
    is.na() |>
    which() |>
    setdiff(index_copy_mutate)
  x[index_echos, type := "echo"]

  # Return early when empty
  if (length(index_copy_mutate) == 0) {
    return(x)
  }

  # We need to distinguish between copy and mutate nodes, because downstream
  # copy nodes will be absorbed my the domain_init nodes, but mutates nodes will
  # not
  mutate_node_ids <- extract_mutate_node_ids(x, index_copy_mutate)
  x[index_copy_mutate, type := "copy"]
  x[node_id %in% mutate_node_ids, type := "mutate"]

  # Identify copy/rename/echo nodes that are
  #   1. external (from different domains than core) and
  #   2. core
  index_copy_mutate_echo <- c(index_copy_mutate,index_echos)
  dep_domains <- vapply(x[["depend_cols"]][index_copy_mutate_echo], function(dc)
    dc[["domain"]], character(1))
  node_copy_mutate_external <- x[["domain"]][index_copy_mutate_echo] != dep_domains &
    dep_domains != "core"
  node_copy_mutate_core <- dep_domains == "core"

  # 1. Update copy/rename/echo nodes having an external dependency with foreign key
  if (any(node_copy_mutate_external)) {
    x <- add_foreign_key_as_depends_col(
      x = x,
      index_copy_mutate_echo = index_copy_mutate_echo,
      node_copy_mutate_external = node_copy_mutate_external,
      pk = pk
    )
  }

  # 2. For copy/rename nodes with a core domain, we need to replace the "core"
  # with the actual name of the domain. This makes downstream processing easier
  if (any(node_copy_mutate_core)) {
    x <- replace_core_with_named_domain(
      x = x,
      index_copy_mutate_echo = index_copy_mutate_echo,
      node_copy_mutate_core = node_copy_mutate_core,
      ui_init = ui_init
    )
  }

  return(x)
}



extract_domain_of_dependency_columns <- function(x) {
  x$depend_cols |>
    lapply(function(i) {
      if (nrow(i) > 1) {
        # Ignore if multiple dependencies are present in which case the action is
        # not a copy, rename, or echo, and needs no update
        return("")
      }
      i$domain
    }) |>
    unlist()
}

extract_mutate_node_ids <- function(x, index_copy_mutate) {
  copy_mutate_nodes <- x[index_copy_mutate]
  copy_mutate_depend_cols <- vapply(copy_mutate_nodes$depend_cols,
                                    `[[`,
                                    "column_name",
                                    FUN.VALUE = character(1L))
  copy_mutate_output_cols <- copy_mutate_nodes$output |> unlist()

  return(copy_mutate_nodes[copy_mutate_depend_cols != copy_mutate_output_cols, node_id])
}

add_foreign_key_as_depends_col <- function(x,
                                           index_copy_mutate_echo,
                                           node_copy_mutate_external,
                                           pk) {
  for (i in index_copy_mutate_echo[node_copy_mutate_external]) {
    domain_i <- x[["domain"]][[i]]
    dep_domain <- x[["depend_cols"]][[i]][["domain"]]

    if (is.null(pk[[toupper(dep_domain)]])) {
      stop(paste0(
        "Domain '",
        dep_domain,
        "' not recognised for foreign key lookup."
      ))
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
  return(x)
}

replace_core_with_named_domain <- function(x,
                                           index_copy_mutate_echo,
                                           node_copy_mutate_core,
                                           ui_init) {
  for (i in index_copy_mutate_echo[node_copy_mutate_core]) {
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
