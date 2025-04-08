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
  # The domain init nodes replace the predecessor nodes
  nodes_to_remove <- domain_init_nodes[, outputs] |>
    extract_("full_name") |>
    unlist()
  nodes_subset <- nodes[!node_id %in% nodes_to_remove]
  nodes_subset[, `:=`(
    core_domains = NA_character_,
    filter_per_domain = NA_character_,
    filter_global = NA_character_
  )]
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
  full_name <- paste0(core_var_tmp$Var2, ".", core_var_tmp$Var1)
  core_variables_i <- data_model_columnn(
    column_name = core_var_tmp$Var1,
    domain = core_var_tmp$Var2,
    full_name = full_name
  )

  outputs_i <- data_model_columnn(
    column_name = core_vars_domain_i,
    domain = nm,
    full_name = paste0(nm, ".", core_vars_domain_i)
  )

  action_name <- "domain_init"

  new_node_i[, `:=`(
    node_id = paste0(nm, ".domain_init"),
    domain = nm,
    action = action_name,
    code_id = NA_character_,
    type = "domain_init",
    depend_cols = list(core_variables_i),
    outputs = list(outputs_i),
    core_domains = list(domain_init_data[[nm]]$core_domains),
    filter_per_domain = list(domain_init_data[[nm]]$filter_domain),
    filter_global = list(domain_init_data[[nm]]$filter_global)
  )]
}
