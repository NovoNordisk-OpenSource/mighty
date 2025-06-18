#' Replace "core" dependencies with domain initialization nodes
#' @description Adds new nodes representing the domain initialization steps, and
#' removes the corresponding nodes that represent the SDTM core variables
#' @param nodes
#'
#' @return
#' @export
#'
#' @examples

create_preprocess_domain_nodes <- function(nodes) {
  nodes_split <- split(nodes, by = "domain")

  # For each domain: Identify the core variables for col_copy/col_mutate nodes
  # core_vars <- nodes_split |>
  dep_vars <- lapply(nodes_split, function(x) {
    # core vars
    core_vars <- lapply(x$depend_cols, function(y) {
      y[domain == "core", ]
    }) |> rbindlist() |> unique()

    is_supp <- x$type == "col_supp"
    supp_vars <- if (any(is_supp)) {
      data.table::data.table(
        column_name = x[is_supp, ]$outputs[[1]],
        domain = x$domain[[1]],
        domain_type = classify_data_domains(x$domain[[1]])
      )
    } else {
      NULL
    }
    rbind(core_vars, supp_vars)
  })

  # Create a preprocess_domain action for each domain
  preprocess_domain_nodes <- purrr::imap(dep_vars,
                                         create_preprocess_domain_node_i,
                                         nodes) |>
    rbindlist()

  # Remove col_copy actions because these are absorbed by preprocess_domain nodes
  is_absorbed_by_preprocess_domain <- nodes$type == "col_copy"
  nodes_retained <- nodes[!is_absorbed_by_preprocess_domain, ]

  # Identify what columns are absorbed by the preprocess_domain actions
  nodes_absorbed <- nodes[is_absorbed_by_preprocess_domain, c("domain", "outputs")]
  nodes_absorbed_list <- paste0(nodes_absorbed$domain, ".", nodes_absorbed$outputs)

  # For the retained actions, redirect any depend_cols that are absorbed by the
  # preprocess_domain actions to the domain_init action
  nodes_retained$depend_cols <- lapply(nodes_retained$depend_cols, function(x) {
    redirect_to_init <- paste0(x$domain, ".", x$column_name) %in% nodes_absorbed_list

    if (any(redirect_to_init)) {
      # If the node is absorbed by a preprocess_domain node, redirect the dependencies
      x$domain_type[redirect_to_init] <- "init"
    }
    x
  })

  # Return the collection of retained actions and preprocess_domain actions
  return(rbind(nodes_retained, preprocess_domain_nodes))
}

create_preprocess_domain_node_i <- function(vars_i,
                                      nm,
                                      nodes) {
  new_node_i <- data.table::data.table(matrix(ncol = ncol(nodes), nrow = 1)) |>
    data.table::setnames(names(nodes))

  new_node_i[, `:=`(
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "preprocess_domain",
    depend_cols = list(vars_i),
    outputs = list(vars_i$column_name)
  )][, node_id := paste0(nm, "-", "preprocess_domain")]
}
