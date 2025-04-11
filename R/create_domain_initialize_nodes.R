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
browser()
  domain_init_nodes <- purrr::imap(core_vars,
                                   create_domain_init_node_i,
                                   nodes,
                                   domain_init_data) |>
    rbindlist()

  # The domain init nodes replace the predecessor nodes
  nodes_to_remove <- domain_init_nodes[, outputs] |>
    extract_("full_name") |>
    unlist()
  nodes_subset <- nodes[!node_id %in% nodes_to_remove]
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

  core_variables_i <- data_model_columnn(
    column_name = core_var_tmp$Var1,
    domain = core_var_tmp$Var2,

  )

  outputs_i <- core_vars_domain_i

  new_node_i[, `:=`(
    node_id = paste0(nm, ".domain_init"),
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "domain_init",
    depend_cols = list(core_variables_i),
    outputs = list(outputs_i)
  )]
}
