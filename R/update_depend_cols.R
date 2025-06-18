#' Update Dependency Columns
#' @description Updates the dependency columns (`depend_cols`) for nodes
#'
#' @details Processes dependencies for two types of actions:
#' - **col_echo actions**: Identifies external dependencies and enriches them with proper
#' foreign key information.
#' - **col_compute actions**: Adds dependencies from other actions that share similar
#'   core column inputs, ensuring proper execution order.
#'
#' @param nodes A `data.table` representing the nodes and their metadata
#' @param pk A named list specifying the primary key columns for external
#'   domains.
#' @param ui_init `ui_init` data structure providing UI initialization details.
#'
#' @returns A modified `data.table` of nodes with enriched `depend_cols`
#'   information.
update_depend_cols <- function(nodes, pk, ui_init) {
  x <- copy(nodes)

  # Enrich depend_cols for col_echo actions having an external dependency with
  # foreign key
  index_echos <- which(x$type == "col_echo")
  if (length(index_echos) > 0) {
    x <- process_external_echo_dependencies(x, index_echos, pk)
  }
  # Enrich depend_cols for col_compute actions that inputs a core column and
  # return the same column in the ADaM domain - core compute actions. The
  # enrichment consists of adding the output columns from all other actions
  # (if any) that have the same core column as input. This will ensure that the
  # latter actions will be executed before the first mentioned col_compute
  # action.
  x <- enrich_core_compute_actions(x)

  return(x)
}



#' Process External Dependencies for col_echo Actions
#' @description Identifies and processes external dependencies in nodes for
#'   col_echo actions.
#'
#' @details Determines which col_echo actions reference external domains and
#'   ensures that the appropriate foreign key columns are included in their
#'   `depend_cols`. This step ensures that external dependencies are correctly
#'   handled during processing.
#'
#' @param x A `data.table` of nodes representing an ADaM domain.
#' @param index_echos An integer vector of indices corresponding to col_echo
#'   actions in the nodes.
#' @param pk A named list specifying primary key columns for external domains.
#'
#' @returns A modified `data.table` of nodes with enriched dependencies for
#'   col_echo actions.
process_external_echo_dependencies <-  function(x, index_echos, pk) {
  # Extract domains from dependencies
  dep_domains <- vapply(x[["depend_cols"]][index_echos], function(dc)
    dc[["domain"]], character(1))

  # Identify external dependencies (not in same domain and not core)
  nodes_echo_external <- x[["domain"]][index_echos] != dep_domains &
    dep_domains != "core"

  # Add foreign keys as dependencies for external dependencies
  if (any(nodes_echo_external)) {
    x <- add_foreign_key_as_depends_col(
      x = x,
      index_echo = index_echos,
      nodes_echo_external = nodes_echo_external,
      pk = pk
    )
  }

  return(x)
}

#' Add Foreign Key Columns to Dependency Columns for External Data Domains
#' @description Adds foreign key columns to `depend_cols` in nodes for actions
#'   with external dependencies.
#'
#' @details Iterates through the specified indices of col_echo actions and
#'   enriches their dependency columns by appending domain-specific foreign key
#'   information. Stops execution if an unrecognized domain is encountered.
#'
#' @param x A `data.table` representing the ADaM nodes structure.
#' @param index_echo An integer vector of indices corresponding to col_echo
#'   actions in the nodes.
#' @param nodes_echo_external A logical vector of the same length as
#'   `index_echo` indicating whether an action references external dependencies.
#' @param pk A named list containing primary key column details for external
#'   domains.
#'
#' @returns A modified `data.table` with enriched foreign key dependency columns
#'   for external actions.
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
        domain_type = c("init", classify_data_domains(dep_domain))
      )
    }) |> rbindlist()

    x[["depend_cols"]][[i]] <- rbind(x[["depend_cols"]][[i]], new_dep_cols)
  }
  return(x)
}


#' Enrich Core Column Dependencies for col_compute Actions
#' @description Enriches `depend_cols` for col_compute actions involving core
#'   columns.
#'
#' @details Identifies col_compute actions that share core column dependencies
#'   and ensures the proper execution order by appending the outputs of other
#'   relevant actions to their dependency columns.
#'
#'   Steps:
#' - Detect core columns used as dependencies and returned as outputs.
#' - For each col_compute action, check if other actions within the same domain share
#'   the same core dependencies.
#' - Append the output columns of these related actions to the `depend_cols` of the
#'   col_compute action being processed.
#'
#' @param x A `data.table` of nodes representing ADaM domain metadata.
#'
#' @returns A modified `data.table` of nodes with enriched `depend_cols` for
#'   col_compute actions.
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
            domain_type = classify_data_domains(domain_i)
          )
          x$depend_cols[[i]] <- rbind(x$depend_cols[[i]], depend_cols_new)
        }
      }
    }
  }
  return(x)
}


