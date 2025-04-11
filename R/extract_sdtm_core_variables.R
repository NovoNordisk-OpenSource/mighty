#' Title
#' @description We want all SDTM core variables. This is determined as the
#' variables that are the parents of "predecessor" nodes. But only predecessor
#' nodes that have a single parent
#'
#' @param nodes
#'
#' @return
#' @export
#'
#' @examples
#'
extract_sdtm_core_variables <- function(nodes) {
  nodes_by_domain <- split(nodes, by = "domain")
  core_variables_by_domain <- lapply(nodes_by_domain, extract_sdtm_core_variables_i)
  names(core_variables_by_domain) <- names(nodes_by_domain)
  core_variables_by_domain
}

extract_sdtm_core_variables_i <- function(nodes_domain_i) {
  browser()
  core_variables <- c()
  for (i in seq_len(nrow(nodes_domain_i))) {
    output_other_nodes <- nodes_domain_i[-i][type != "row", outputs] |>
      extract_("full_name") |>
      unlist()
    potentials <- nodes_domain_i[i, depend_cols] |>
      extract_("full_name") |>
      unlist() |>
      setdiff(output_other_nodes) |>
      unique()
    core_variables_i <- potentials[grepl(pattern = paste0("^", nodes_domain_i$domain[[1]] , "\\."), potentials,
                                         ignore.case = TRUE)]
    core_variables <- c(core_variables, core_variables_i)

  }

  sub("^.*\\.", "", core_variables) |>
    unique() |>
    sort()
}




extract_sdtm_core_variables_2 <- function(nodes) {
  # This is the same as the previous function, but uses a different approach
  # to extract the core variables
  nodes_by_domain <- split(nodes, by = "domain")
  core_variables_by_domain <- lapply(nodes_by_domain, extract_sdtm_core_variables_i_2)
  names(core_variables_by_domain) <- names(nodes_by_domain)
  core_variables_by_domain
}

extract_sdtm_core_variables_i_2 <- function(nodes_domain_i) {
  core_variables <- c()

  for (i in seq_len(nrow(nodes_domain_i))) {

    node_i <- nodes_domain_i[i, ]
    if(node_i$type == "row") {
      next
    }
    depend_cols <- node_i$depend_cols[[1]]
    outputs <- node_i$outputs[[1]]
    # Node has only a single parent and single child
    single <- (nrow(depend_cols) == nrow(outputs)) == 1
    if (!single) {
      next
    }
    # Parent is identical to child
    parent_same_as_child <- all(depend_cols == outputs)
    if (!parent_same_as_child) {
      next
    }
    # Parent/child must reference "self"
    is_self <- depend_cols$domain == "self"
    if (!is_self) {
      next
    }
    core_variables <- c(core_variables, depend_cols$column_name)
  }
  core_variables |> sort()
}


# Foreign dependencies that are not predecessor variables
extract_foreign <- function(nodes) {
  # Collect all depends vars
  extract_foreign_i <- function(domain_i) {
    keep_vars <- lapply(domain_i, function(i) {
      i$depends
    }) |> unlist(recursive = FALSE) |> unname() |> toupper()
    keep_vars[!grepl("^SELF\\.", keep_vars)] |> unique() |> sort()
  }
  lapply(nodes, extract_foreign_i)
}

add_initial_predecessors_to_metadata <- function(metadata_init, init_predecessors) {
  metadata_init <- purrr::map2(
    metadata_init,
    init_predecessors,
    .f = function(yml, pred) {
      yml[["initial_predecessors"]] <- pred
      yml
    }
  )
  metadata_init
}
