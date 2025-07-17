#' Generate Code for Individual Nodes
#' @description Generates executable code for each node within a program based
#'   on node type and specifications.
#'
#' @param actions_program_i Data table containing nodes for a specific program
#'   with columns for type, domain, outputs, dependencies, and parameters
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns
#' @param std_code_env Environment containing code components
#' @param ui_data List with domain-specific configurations and filter
#'   dependencies
#' @param trial_metadata List containing trial-specific metadata including data
#'   paths and connection information
#'   study
#' @param path_output Character string specifying the output path where
#'   generated data should be stored
#'
#' @returns A list containing generated code blocks for each node in the
#'   program, with each element representing the code for one processing step
generate_action_code <- function(actions_program_i,
                                 domain_keys,
                                 code_component_envr,
                                 ui_data,
                                 trial_metadata,
                                 path_output) {
  program <- list()

  for (i in seq_len(nrow(actions_program_i))) {
    action_i <- actions_program_i[i]

    if (action_i$code_id == "_read_data") {
      external_deps <- action_i$outputs[[1]]
      init <- ui_data[[action_i$domain]]$init
      program[[i]] <- generate_read_data_code2(
        external_deps,
        path_output = path_output
      )
      next
    }
    if (action_i$type == "initialize_domain") {
      init <- ui_data[[action_i$domain]]$init
      program[[i]] <- generate_initialize_domain(
        .self = action_i$domain,
        base_domains = init$base_domains,
        domain_filters_exist = any(!is.na(unlist(init$filter_domain)))
      )
      next
    }
    if (action_i$type == "preprocess_domain") {
      domain_metadata <- ui_data[[action_i$domain]]$init

      program[[i]] <- generate_preprocess_domain(
        .self = action_i$domain,
        base_domains = domain_metadata$base_domains,
        adsl_domain_keys = domain_keys$ADSL,
        filter_domain = domain_metadata$filter_domain,
        filter_global = domain_metadata$filter_global,
        filter_depend_cols = domain_metadata$filter_depend_cols,
        keep_vars = action_i$outputs[[1]]
      )
      next
    }
    if (action_i$type == "read_domain") {
      program[[i]] <- generate_read_domain(
        adam_domain = action_i$domain
      )
      next
    }
    if (grepl("col_mutate|col_echo", action_i$type)) {
      is_mutate <- action_i$depend_cols[[1]] |> nrow() == 1
      if (is_mutate) {
        depends <- action_i[["depend_cols"]][[1]][["column_name"]]
        outputs <- action_i[["outputs"]][[1]]
        program[[i]] <- generate_mutate_code(
          .self = node_i$domain,
          rename_var = outputs,
          source_var = depends,
          node_id = action_i$node_id

        )
        next
      }

      depend_columns <- action_i[["depend_cols"]][[1]][["column_name"]]
      depend_domains <- action_i[["depend_cols"]][[1]][["domain"]]
      outputs <- action_i[["outputs"]][[1]]
      x <- pre_process_generate_rename_left_join_code(depend_columns,
                                                      depend_domains,
                                                      outputs,
                                                      action_i$domain,
                                                      domain_keys)
      program[[i]] <- generate_rename_left_join_code(
        .self = action_i$domain,
        join_dataset = x$join_dataset,
        var_to_add = x$var_to_add,
        by_vars = x$by_vars,
        node_id = action_i$node_id,
        output_var = x$output_var
      )
      next

    }
    if (action_i$type %in% c("col_compute", "col_supp", "row_compute")) {
      program[[i]] <- parse_into_chunks(
        code_id = action_i$code_id,
        user_supplied_parameters = action_i$parameters |> unlist(FALSE),
        node_id = action_i$node_id,
        domain_name = action_i$domain,
        outputs = action_i$outputs,
        env = code_component_envr
      )
      next
    }
    if (action_i$type == "write_domain") {
      # Collect input table names
      if (any(actions_program_i$type == "read_data")) {
        input_tables <- actions_program_i[actions_program_i$type == "read_data", ]$input_cols[[1]][["domain"]] |>
          unique()
      } else {
        input_tables <- c()
      }

      program[[i]] <- generate_write_domain(
        domain_name = action_i$domain,
        path_output = path_output,
        input_tables
      ) |> paste0(collapse = "\n\n")
      next
    }

    stop("Unknown node type")

  }
  return(program)
}
