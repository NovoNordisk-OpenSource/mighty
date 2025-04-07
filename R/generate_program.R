generate_program <- function(program_order,
                             nodes,
                             domain_keys,
                             std_library_path,
                             trial_metadata,
                             data_connection) {
  # Merge the program_id and rank column from program_order onto nodes
  # data.table to get the program_id for each node. Then sort the nodes by
  # program_id and rank.
  keep_only_from_program_order <- c("type", "domain")
  nodes <- merge(program_order[, .(node_id,
                                   domain,
                                   program_id,
                                   rank,
                                   type,
                                   external_dependencies_by_program)], nodes[, !..keep_only_from_program_order], by = "node_id", all.x = TRUE) |>
    setorder(program_id, rank)
  # Create clean, empty environment to store standard components
  std_code_env <- new.env()
  std_library_path |>
    lapply(source, local = std_code_env)

  sdtm_dataset_list <- list_all_(type = "sdtm", trial_metadata)
  adam_dataset_list <- list_all_(type = "adam", trial_metadata)

  nodes_split <- split(nodes, by = "program_id")

  programs <- lapply(
    nodes_split,
    generate_node_code,
    domain_keys,
    std_code_env,
    trial_metadata,
    sdtm_dataset_list,
    adam_dataset_list,
    data_connection
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


list_all_sdtm_datasets <- function(trial_metadata) {
  # Generate list of all SDTM datasets in the current study so later we can
  # check if a specific supp dataset exists
  
  sdtm_path <-
    sdtm_connector <- connector::connector_fs()(path = sdtm_path)
  sdtm_connector |> connector::list_content_cnt()

}
list_all_ <- function(type = c("sdtm", "adam"), trial_metadata) {
  # Generate list of all SDTM datasets in the current study so later we can
  # check if a specific supp dataset exists
  if (type == "adam") {
    path <- glue::glue(
      "~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/current/stats/data/adam"
    )
  }
  if (type == "sdtm") {
    path <- glue::glue(
      "~/projstat/{trial_metadata$project_id}/{trial_metadata$complete_id}/current/dm/data/sdtm"
    )
  }

  # This is needed when testing and referencing trial data locations that don't
  # exist, or the testing environment doesn't have access to
  result <- tryCatch({
    
    connector::connector_fs(path = path) |>
      connector::list_content_cnt()
  }, error = function(e) {
    if (grepl("directory.*does not.*exist", e$message, ignore.case = TRUE)) {
      return(NULL)
    } else {
      stop(e)  # re-throws the original error for all other cases
    }
  })
  return(result)
}


make_adam_domain_ext <- function(adam_domain,
                                 file_extension,
                                 adam_dataset_list) {
  adam_domain_ext <- paste(adam_domain, file_extension[1], sep = ".")
  parquet_exists <- adam_domain_ext %in% adam_dataset_list
  if (!parquet_exists) {
    adam_domain_ext <- paste(adam_domain, file_extension[2], sep = ".")
  }
  return(adam_domain_ext)
}
