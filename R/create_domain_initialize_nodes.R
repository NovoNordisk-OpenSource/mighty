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

  nodes_split <- split(nodes, by = "domain")

  # Identify the core variables for predecessors for each domain
  core_vars <- lapply(nodes_split, function(x) {

    # Identify column dependencies for all predecessors
    domain_i <- x$domain[[1]]
    core_domains <- domain_init_data[[domain_i]][["core_domains"]]
    x_sub <- x[x$type == "predecessor", ]
    dep_cols <- x_sub$depend_cols
    names(dep_cols) <- unlist(x_sub$outputs)

    # Check that all domains are either core domain(s) (internal predecessor)
    # or none (external predecessor) and ignore external predecessors
    dep_cols_core <- lapply(dep_cols, function(y) {
      domain_match <- y$domain %in% core_domains
      if(!(all(domain_match) | all(!domain_match))) {
        stop(paste("Invalid domain(s)",
                   paste0(setdiff(y$domain, core_domains), collapse = ','),
                   "in predecessor node"))
      }
      if(all(!domain_match)){
        return(NULL)
      }
      y
    })

    # Remove NULL values from the list
    return(dep_cols_core[!unlist(lapply(dep_cols_core, is.null))])
  })

  # Create a domain_init action for each domain
  domain_init_nodes <- purrr::imap(lapply(core_vars, rbindlist),
                                   create_domain_init_node_i,
                                   nodes,
                                   domain_init_data) |>
    rbindlist()


  # Remove actions consumed by domain_init nodes
  nodes_to_remove <- lapply(names(core_vars), function(nm) {
    ls1 <- lapply(core_vars[[nm]], function(y) unique(y$column_name))
    paste0(nm, "-", names(ls1)[names(ls1) == ls1])
  }) |> unlist()
  nodes_subset <- nodes[!nodes$node_id %in% nodes_to_remove,]

  # Update core predecessors that are not consumed by domain_init actions
  # These are predecessors from core domain(s) that are renamed and thus must
  # have their own separate actions. The update is done by replacing the domain
  # name in the depend_cols with the ADaM domain name.
  for(i in seq_len(nrow(nodes_subset))){
    if(nodes_subset[["type"]][[i]] == "predecessor"){
      dep_cols <- nodes_subset[["depend_cols"]][[i]]
      domain_i <- nodes_subset[["domain"]][[i]]
      idx_mod <- dep_cols$domain %in% domain_init_data[[domain_i]][["core_domains"]]

      # Only core predecessors are modified
      if(any(idx_mod)){
        dep_cols[["domain"]][idx_mod] <- domain_i
        dep_cols[["domain_type"]][idx_mod] <- "adam"
        nodes_subset[["depend_cols"]][[i]] <- unique(dep_cols)
      }
    }
  }

  # Return the updated nodes with domain_init nodes
  return(rbind(nodes_subset, domain_init_nodes))
}


create_domain_init_node_i <- function(core_vars_domain_i,
                                      nm,
                                      nodes,
                                      domain_init_data) {

  new_node_i <- data.table::data.table(matrix(ncol = ncol(nodes), nrow = 1)) |>
    data.table::setnames(names(nodes))

  new_node_i[, `:=`(
    domain = nm,
    code_id = NA_character_,
    depend_rows = NA_character_,
    parameters = NA_character_,
    type = "domain_init",
    depend_cols = list(core_vars_domain_i),
    outputs = list(unique(core_vars_domain_i$column_name))
  )][, node_id := paste0(domain, "-", "domain_init")]
}

