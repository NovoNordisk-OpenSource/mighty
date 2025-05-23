#' @title Update update_depend_cols
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
update_depend_cols <- function(nodes, pk, ui_init) {
  x <- copy(nodes)

  # Enrich depend_cols for col_echo actions having an external dependency with
  # foreign key
  index_echos <- which(x$type == "col_echo")
  if (length(index_echos) > 0) {
    dep_domains <- vapply(x[["depend_cols"]][index_echos], function(dc)
      dc[["domain"]], character(1))
    nodes_echo_external <- x[["domain"]][index_echos] != dep_domains &
      dep_domains != "core"
    if (any(nodes_echo_external)) {
      x <- add_foreign_key_as_depends_col(
        x = x,
        index_echo = index_echos,
        nodes_echo_external = nodes_echo_external,
        pk = pk
      )
    }
  }

  # Enrich depend_cols for col_compute actions that inputs a core column and
  # return the same column in the ADaM domain - core compute actions. The
  # enrichment consists of adding the output columns from all other actions
  # (if any) that have the same core column as input. This will ensure that the
  # latter actions will be executed before the first mentioned col_compute
  # action.
  x <- enrich_core_compute_actions(x)

  # Replace "core" domain with the actual name of the core domain(s) "-tmp"
  # This makes downstream processing easier
  x <- replace_core_with_named_domain(x, ui_init)

  return(x)
}


extract_domain_of_dependency_columns <- function(x) {
  x$depend_cols |>
    lapply(function(i) {
      if (nrow(i) > 1) {
        # Ignore if multiple dependencies are present in which case the action is
        # not a col_copy, col_mutate, or col_echo, and needs no update
        return("")
      }
      i$domain
    }) |>
    unlist()
}


add_foreign_key_as_depends_col <- function(x,
                                           index_echo,
                                           nodes_echo_external,
                                           pk) {
  for (i in index_echo[nodes_echo_external]) {
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

replace_core_with_named_domain <- function(x, ui_init) {

  for (i in seq_len(nrow(x))) {

    # Extract the dependency columns and domain for the current node
    dep_cols_i <- x[["depend_cols"]][[i]]
    domain_i <- x[["domain"]][[i]]
    core_domains <- ui_init[[domain_i]][["core_domains"]]

    # Check if any dependencies are core
    is_core_dep <-  tolower(dep_cols_i$domain) == "core"

    # Keep non-core dependencies unchanged
    retained_dep_cols <- dep_cols_i[!is_core_dep,]

    # Replace core domain with actual domain(s)
    if(any(is_core_dep)) {
      replaced_dep_cols <- expand.grid(
        "domain" = core_domains,
        "column_name" = dep_cols_i$column_name[is_core_dep],
        stringsAsFactors = FALSE
      )
      replaced_dep_cols[["domain_type"]] = "temp"
    } else {
      replaced_dep_cols <- NULL
    }

    # Combine retained and replaced dependency columns
    x[["depend_cols"]][[i]] <- rbind(retained_dep_cols, replaced_dep_cols)
  }

  return(x)

}

enrich_core_compute_actions <- function(x) {

  # Identify depend_cols that are of domain "core"
  dep_core_cols <- lapply(x$depend_cols,
                          function(y){y[domain == "core",][["column_name"]]})
  seq_x <- seq_len(nrow(x))

  # Loop over each action
  for (i in seq_x) {

    # Check if the action is a col_compute
    if (x$type[[i]] == "col_compute") {

      # Check if the action has core dependencies that are returned as output
      core_vars_returned_i <- intersect(x$outputs[[i]][[1]], dep_core_cols[[i]])
      domain_i <- x$domain[[i]]

      # If so, then check if there are any other actions that have the same
      # core dependencies and are in the same domain as the current action
      if (length(core_vars_returned_i) > 0) {
        has_same_core_dep_i <- lapply(seq_x, function(j) {
          i != j &&
            domain_i == x$domain[[j]] &&
            any(core_vars_returned_i %in% dep_core_cols[[j]])
        }) |> unlist()

        # If there are any such actions, then add their output columns to the
        # depend_cols of the current action
        if (any(has_same_core_dep_i)) {
          depend_cols_new <- data.table(
            column_name = unlist(x$output[has_same_core_dep_i]),
            domain = domain_i,
            domain_type = classify_external_data_domains(domain_i)
          )
          x$depend_cols[[i]] <- rbind(x$depend_cols[[i]], depend_cols_new)
        }
      }
    }
  }
  return(x)
}


