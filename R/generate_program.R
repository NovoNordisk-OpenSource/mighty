#' Generate Program Code
#' @description Generates complete program code by processing nodes in specified
#'   order and combining them into executable programs.
#'
#' @details Merges program ordering information with node data, sorts nodes by
#'   program and rank, then generates code for each program by processing nodes
#'   sequentially.
#'
#' @param program_order Data table containing program execution order with
#'   columns for node_id, domain, program_id, rank, type, and external
#'   dependencies
#' @param nodes Data table containing node definitions and specifications
#' @param domain_keys Named list mapping domain names to their respective key
#'   columns
#' @param code_component_env Environment containing code components
#' @param trial_metadata List containing trial-specific metadata including data
#'   paths and connection information
#' @param ui_data List
#' @param path_output Optional character string specifying the output path where
#'   generated programs and data should be stored.
#'
#' @returns A named list of generated programs where each element contains the
#'   complete code for one program, with names in the format "program_id_domain"
generate_program <- function(program_order,
                             nodes,
                             domain_keys,
                             code_component_env,
                             trial_metadata,
                             ui_data,
                             path_output = NULL) {
  # Merge the program_id and rank column from program_order onto nodes
  # data.table to get the program_id for each node. Then sort the nodes by
  # program_id and rank.
  keep_only_from_program_order <- c("type", "domain")
  nodes_and_programs <- merge(program_order[, .(node_id,
                                   domain,
                                   program_id,
                                   rank,
                                   type,
                                   input_cols)], nodes[, !..keep_only_from_program_order], by = "node_id", all.x = TRUE) |>
    setorder(program_id, rank)
  # Create clean, empty environment to store standard components
  sdtm_dataset_list <- list_all_(type = "sdtm", trial_metadata, path_output)
  nodes_split <- split(nodes_and_programs, by = "program_id")

  programs <- lapply(
    nodes_split,
    generate_node_code,
    domain_keys,
    code_component_env,
    ui_data,
    trial_metadata,
    sdtm_dataset_list,
    path_output
  )

  programs <- rename_programs(programs, nodes_split)

  return(programs)
}


rename_programs <- function(programs, nodes_split) {
  current_names <- names(programs)
  program_domains <- vapply(nodes_split, function(x) {
    x$domain[1]
  }, character(1))
  new_names <- paste0(current_names, "_", program_domains)
  names(programs) <- new_names
  return(programs)
}



list_all_ <- function(type = c("sdtm", "adam"), trial_metadata, custom_data_path = NULL) {
  # Generate list of all SDTM datasets in the current study so later we can
  # check if a specific supp dataset exists
  # TODO: The above mentions SUPPDM - should this function be general and keep the type, or do we only
  # need it for validating precense of SDTM datasets?
  cnt <- eval(parse(text = glue::glue("connector::connect(config = '{custom_data_path}/_connector.yml')${type}")))

  # This is needed when testing and referencing trial data locations that don't
  # exist, or the testing environment doesn't have access to
  result <- tryCatch({
    cnt |> connector::list_content_cnt()
    }, error = function(e) {
      if (grepl("directory.*does not.*exist", e$message, ignore.case = TRUE)) {
        return(NULL)
      } else {
        stop(e)  # re-throws the original error for all other cases
      }
    })
  return(result)
}
