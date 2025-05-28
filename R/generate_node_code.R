#' @title Generate Node Code
#'
#' @param nodes_program_i
#' @param domain_keys
#' @param std_code_env
#' @param ui_data
#' @param trial_metadata
#' @param sdtm_dataset_list
#' @param data_connection
#' @param connector_config_path
#' @param path_output
#'
#' @return
#' @export
#'
#' @examples
generate_node_code <- function(nodes_program_i,
                               domain_keys,
                               std_code_env,
                               ui_data,
                               trial_metadata,
                               sdtm_dataset_list,
                               data_connection,
                               connector_config_path,
                               path_output) {
  program <- list()

  for (i in seq_len(nrow(nodes_program_i))) {
    node_i <- nodes_program_i[i]

    if (node_i$type == "external") {
      external_deps <- node_i$external_dependencies_by_program[[1]]
      program[[i]] <- generate_external_data_code(
        external_deps,
        trial_metadata,
        sdtm_dataset_list,
        data_connection,
        connector_config_path,
        path_output = path_output
      ) |> paste0(collapse = "\n\n")

      next
    }
    if (node_i$type == "domain_init") {
      domain_metadata <- ui_data[[node_i$domain]]$init
      filter_depend_cols <- domain_metadata$filter_depend_cols
      adsl_name <- regmatches(filter_depend_cols,
                              regexpr("ADSL|adsl", filter_depend_cols)) |>
        unique()




      program[[i]] <- generate_initialize_domain(
        .self = node_i$domain,
        core_domains = domain_metadata$core_domains,
        adsl_domain_keys = domain_keys$ADSL,
        adsl_name = adsl_name,
        filter_domain = domain_metadata$filter_domain,
        filter_global = domain_metadata$filter_global,
        keep_vars = node_i$outputs[[1]]
      )
      next
    }
    if (node_i$type == "program_init") {
      program[[i]] <- generate_program_init(
        adam_domain = node_i$domain,
        adam_dataset_list = adam_dataset_list,
        data_connection = data_connection,
        path_out = path_output

      )
      next
    }

    if (grepl("col_mutate|col_echo", node_i$type)) {
      is_mutate <- node_i$depend_cols[[1]] |> nrow() == 1
      if (is_mutate) {
        depends <- node_i[["depend_cols"]][[1]][["column_name"]]
        outputs <- node_i[["outputs"]][[1]]
        program[[i]] <- generate_mutate_code(
          .self = node_i$domain,
          rename_var = outputs,
          source_var = depends,
          node_id = node_i$node_id

        )
        next
      }

      depend_columns <- node_i[["depend_cols"]][[1]][["column_name"]]
      depend_domains <- node_i[["depend_cols"]][[1]][["domain"]]
      outputs <- node_i[["outputs"]][[1]]
      x <- pre_process_generate_rename_left_join_code(depend_columns,
                                                      depend_domains,
                                                      outputs,
                                                      node_i$domain,
                                                      domain_keys)
      program[[i]] <- generate_rename_left_join_code(
        .self = node_i$domain,
        join_dataset = x$join_dataset,
        var_to_add = x$var_to_add,
        by_vars = x$by_vars,
        node_id = node_i$node_id,
        output_var = x$output_var
      )
      next

    }
    if (node_i$type == "col_compute" || node_i$type == "row_compute") {
      program[[i]] <- parse_into_chunks(
        code_id = node_i$code_id,
        user_supplied_parameters = node_i$parameters |> unlist(FALSE),
        node_id = node_i$node_id,
        domain_name = node_i$domain,
        outputs = node_i$outputs,
        env = std_code_env
      )
      next
    }
    if (node_i$type == "write_data") {
      # Collect input table names
      if (any(nodes_program_i$type == "external")) {
        input_tables <- nodes_program_i[nodes_program_i$type == "external", ]$external_dependencies_by_program[[1]][["domain"]] |>
          unique()
      } else {
        input_tables <- c()
      }

      program[[i]] <- generate_write_data(
        domain_name = node_i$domain,
        data_connection = data_connection,
        path_output = path_output,
        input_tables
      ) |> paste0(collapse = "\n\n")
      next
    }

    stop("Unknown node type")

  }
  return(program)
}
