#' Replace "core" dependencies with domain initialization nodes
#' @description Adds new nodes representing the domain initialization steps, and
#' removes the corresponding nodes that represent the SDTM core variables
#' @param nodes
#'
#' @return
#' @export
#'
#' @examples

create_domain_initialize_nodes <- function(nodes) {
  nodes_split <- split(nodes, by = "domain")

  # For each domain: Identify the core variables for col_copy/col_mutate nodes
  # core_vars <- nodes_split |>
  core_vars <- lapply(nodes_split, function(x) {
    lapply(x$depend_cols, function(y) {
      y[domain == "core",]
    }) |> rbindlist() |> unique()
  })

  # Create a domain_init action for each domain
  domain_init_nodes <- purrr::imap(core_vars,
                                   create_domain_init_node_i,
                                   nodes) |>
    rbindlist()

  # Remove col_copy actions because these are absorbed by domain_init nodes
  is_absorbed_by_init <- nodes$type == "col_copy"
  nodes_retained <- nodes[!is_absorbed_by_init, ]

  # Identify what columns are absorbed bu the domain_init actions
  nodes_absorbed <- nodes[is_absorbed_by_init, c("domain", "outputs")]
  nodes_absorbed_list <- paste0(nodes_absorbed$domain, ".", nodes_absorbed$outputs)

  # For the retained actions, redirect any depend_cols that are absorbed by the
  # domain_init actions to the domain_init action
  nodes_retained$depend_cols <- lapply(nodes_retained$depend_cols, function(x) {
    redirect_to_init <- paste0(x$domain, ".", x$column_name) %in% nodes_absorbed_list

    if (any(redirect_to_init)) {
      # If the node is absorbed by a domain_init node, redirect the dependencies
      x$domain_type[redirect_to_init] <- "init"
    }
    x
  })

  # Return the collection of retained actions and domain_init actions
  return(rbind(nodes_retained, domain_init_nodes))
}







create_domain_init_node_i <- function(core_vars_domain_i,
                                      nm,
                                      nodes) {
  new_node_i <- data.table::data.table(matrix(ncol = ncol(nodes), nrow = 1)) |>
    data.table::setnames(names(nodes))

  new_node_i[, `:=`(
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "domain_init",
    depend_cols = list(core_vars_domain_i),
    outputs = list(core_vars_domain_i$column_name)
  )][, node_id := paste0(nm, "-", "domain_init")]
}
