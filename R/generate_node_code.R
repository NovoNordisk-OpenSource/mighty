#' @title Generate Node Code
#'
#' @param nodes_program_i
#' @param domain_keys
#' @param std_code_env
#' @param trial_metadata
#' @param sdtm_dataset_list
#' @param adam_dataset_list
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_node_code <- function(nodes_program_i,
                               domain_keys,
                               std_code_env,
                               ui_data,
                               sdtm_dataset_list,
                               adam_dataset_list,
                               data_connection) {
  program <- list()

  trial_metadata <- ui_data$trial_metadata

  for (i in seq_len(nrow(nodes_program_i))) {
    node_i <- nodes_program_i[i]

    if (node_i$type == "external") {
      external_deps <- node_i$external_dependencies_by_program[[1]]
      program[[i]] <- generate_external_data_code(external_deps,
                                                  trial_metadata,
                                                  sdtm_dataset_list,
                                                  adam_dataset_list,
                                                  data_connection) |> paste0(collapse = "\n\n")

      next
    }
    if (node_i$type == "domain_init") {
      domain_metadata <- ui_data$init[[node_i$domain]]
      program[[i]] <- generate_initialize_domain(
        .self = node_i$domain,
        core_domains = domain_metadata$core_domains,
        domain_filter = domain_metadata$filter_domain,
        filter_global = domain_metadata$filter_global,
        keep_vars = node_i$outputs[[1]]$column_name
      )
      next
    }
    if (node_i$type == "program_init") {
      program[[i]] <- generate_program_init(adam_domain = node_i$domain,
                                            adam_dataset_list = adam_dataset_list,
                                            data_connection = data_connection)
      next
    }
    if (node_i$type == "predecessor") {
      is_mutate <- node_i$depend_cols[[1]] |> nrow() == 1
      if (is_mutate) {
        depends <- node_i[["depend_cols"]][[1]][["column_name"]]
        outputs <- node_i[["outputs"]][[1]][["column_name"]]
        program[[i]] <- predecessor_mutate(
          .self = node_i$domain,
          rename_var = outputs,
          source_var = depends,
          node_id = node_i$node_id
        )
        next
      }
      depends <- node_i[["depend_cols"]][[1]][["full_name"]]
      outputs <- node_i[["outputs"]][[1]][["full_name"]]
      x <- pre_process_predecessor_left_join(depends, outputs, node_i$domain, domain_keys)
      program[[i]] <- predecessor_left_join(
        .self = node_i$domain,
        join_dataset = x$join_dataset,
        var_to_add = x$var_to_add,
        by_vars = x$by_vars,
        node_id = node_i$node_id,
        output_var = x$output_var
      )
      next

    }
    if (node_i$type == "derivation" || node_i$type == "row") {
      program[[i]] <- parse_into_chunks(
        code_id = node_i$code_id,
        node_id = node_i$node_id,
        domain_name = node_i$domain,
        env = std_code_env
      )
      next
    }
    stop("Unknown node type")

  }
  return(program)
}
