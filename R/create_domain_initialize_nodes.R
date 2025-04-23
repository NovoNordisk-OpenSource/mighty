#' Replace "core" dependencies with domain initialization nodes
#' @description Adds new nodes representing the domain initialization steps, and
#' removes the corresponding nodes that represent the SDTM core variables
#' @param nodes
#' @param domain_init_data
#'
#' @return
#' @export
#'
#' @examples

create_domain_initialize_nodes <- function(nodes, domain_init_data) {

  core_vars <- extract_sdtm_core_variables(nodes)
  domain_init_nodes <- purrr::imap(core_vars,
                                   create_domain_init_node_i,
                                   nodes,
                                   domain_init_data) |>
    rbindlist()

  # The domain init nodes replace the predecessor nodes for core variables Need
  # a data.table of dommain - column for both domain init nodes, and regular
  # nodes Only nodes that have 1 depend_col are eligible, as those with multiple
  # are either predecessor with renaming or derivations

  nodes_to_remove <- domain_init_nodes[, outputs]  |>
    purrr::map2(domain_init_nodes$domain, function(i, domain) {
      paste0(domain, "-", i)
    }) |> unlist()

  # Nodes having only a single output & are predecessor nodes
  inx_single_dependency <- vapply(nodes$depend_cols, function(i)
    nrow(i) == 1, FUN.VALUE = logical(1L))
  inx_pred <- nodes[, type == "predecessor"]
  inx <- inx_single_dependency & inx_pred

  # Make a temporary ID to match against the nodes_to_remove
  nodes_temp <- copy(nodes)
  nodes_temp[, domain_init_cols_tmp := NA_character_]
  nodes_temp[inx, domain_init_cols_tmp := paste0(domain, "-", outputs)]

  nodes_subset <- nodes_temp[!domain_init_cols_tmp %in% nodes_to_remove]
  nodes_subset[, domain_init_cols_tmp := NULL]
  rbind(nodes_subset, domain_init_nodes)
}


create_domain_init_node_i <- function(core_vars_domain_i,
                                      nm,
                                      nodes,
                                      domain_init_data) {
  new_node_i <- data.table::data.table(matrix(ncol = ncol(nodes), nrow = 1)) |>
    data.table::setnames(names(nodes))
  core_var_tmp <- expand.grid(core_vars_domain_i,
                              domain_init_data[[nm]]$core_domains,
                              stringsAsFactors = FALSE)

  core_variables_i <- data_model_columnn(column_name = core_var_tmp$Var1, domain = core_var_tmp$Var2)

  new_node_i[, `:=`(
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "domain_init",
    depend_cols = list(core_variables_i),
    outputs = list(core_vars_domain_i)
  )][, node_id := paste0(domain, "-", "domain_init")]
}

