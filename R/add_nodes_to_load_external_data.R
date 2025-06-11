#' Update program order with nodes to load external data
#'
#' @param program_order
#' @param nodes
#' @param init_metadata
#' @param domain_keys
#'
#' @return
#'
#' @examples
add_nodes_to_load_external_data <- function(program_order, nodes, init_metadata, domain_keys) {
  external_deps <- external_dependencies_per_program(program_order, nodes, init_metadata, domain_keys)
  x <- split(program_order, by = "program_id")

  new_nodes_list <- purrr::imap(x, function(i, nm) {
    deps_i <- external_deps[[nm]]
    if (is.null(deps_i))
      return(i)
    domain_i <- unique(i$domain)
    new_node <- data.table::data.table(matrix(ncol = ncol(i))) |> setnames(names(i))
    new_node[, `:=`(
      domain = domain_i,
      node_id =  paste0(domain_i, "-", i$program_id[1], "-read_data"),
      program_id = i$program_id[1],
      rank =
        i[type=="domain_init"|type=="program_init", rank]-0.5,
      type = "read_data",
      external_dependencies_by_program = list(deps_i)
    )]

  }) |>
    rbindlist()
  prog_order_tmp <- copy(program_order)
  prog_order_tmp[, external_dependencies_by_program := list(NA_character_)]
  out <- rbindlist(list(prog_order_tmp, new_nodes_list)) |>
    setorder(program_id, rank)
  out[, rank := .I]
}



#' Get external dependencies per program
#'
#' @param program_order
#' @param nodes
#' @param init_metadata
#'
#' @return
#'
#' @examples
external_dependencies_per_program <- function(program_order, nodes, init_metadata, domain_keys) {

  x <- program_order[nodes[, .(node_id, depend_cols)], on = .(node_id)] |>
    setorder(program_id, rank)
  nodes_by_pgm <- split(x[, .(node_id,  domain, type, depend_cols, program_id)], by =
                          "program_id")
  dep_src_cols_by_pgm <- lapply(nodes_by_pgm, function(i) {
    i[, rbindlist(depend_cols), by = node_id] |>
      dplyr::filter(domain !=  unique(i$domain)) |>
      dplyr::mutate(domain_type = ifelse(domain_type == "init",
                                         classify_data_domains(domain),
                                         domain_type))
  })

  ext_cols_by_pgm <- lapply(seq_len(length(nodes_by_pgm)), function(i){

    y <- nodes_by_pgm[[i]]

    if(any(y$type == "domain_init")){
      filter_depend_cols <- init_metadata[[unique(y$domain)]][["filter_depend_cols"]]
      core_domains <- init_metadata[[unique(y$domain)]][["core_domains"]]

      # Create template for empty dependencies
      dep_empty <- data.table::data.table(
        domain = character(),
        domain_type = character(),
        column_name = character()
      )

      # Column dependencies coming from ADSL
      filter_depend_cols_adsl <- filter_depend_cols[grepl("^ADSL\\.", filter_depend_cols, ignore.case = TRUE)]
      adsl_name <- lapply(filter_depend_cols_adsl, function(x) strsplit(x, "\\.")[[1]][[1]]) |> unlist() |> unique()
      if(length(adsl_name) > 1){
        stop(paste("Inconsistent casing of ADSL in domain filter for", toupper(y$domain), "."))
      }

      if (length(filter_depend_cols_adsl) > 0) {
        dep_adsl <- data.table::data.table(domain = adsl_name,
                                           domain_type = "adam",
                                           column_name = gsub("^ADSL\\.", "", filter_depend_cols_adsl, ignore.case = TRUE))
      } else {
        dep_adsl <- dep_empty
      }

      # If ADSL is required in the core filter, then add foreign key to filter dependency
      if (nrow(dep_adsl) > 0) {
        dep_key <- expand.grid(
          "domain" = c(adsl_name, core_domains),
          "column_name" = domain_keys[["ADSL"]],
          stringsAsFactors = FALSE
        )
        dep_key[["domain_type"]] <- classify_data_domains(dep_key[["domain"]])
      } else {
        dep_key <- dep_empty
      }

      # Column dependencies coming from the "core" domain(s)
      filter_depend_cols_core <- gsub("^core\\.", "", filter_depend_cols[grepl("^core\\.", filter_depend_cols, ignore.case = TRUE)])
      dep_core <- expand.grid(
        "domain" = core_domains,
        "column_name" = filter_depend_cols_core,
        stringsAsFactors = FALSE
      ) |> as.data.table()
      dep_core[["domain_type"]] <- classify_data_domains(dep_core[["domain"]])

      # Combine the dependencies
      out <- rbind(dep_core, dep_adsl, dep_key, dep_src_cols_by_pgm[[i]][, c("domain", "domain_type", "column_name")])

    } else {
      out <- dep_src_cols_by_pgm[[i]][, c("domain", "domain_type",  "column_name")]
    }
    out <- out |>
      unique() |>
      setorder(domain_type, domain, column_name) |>
      setcolorder(c("domain", "domain_type", "column_name"))
    row.names(out) <- seq_len(nrow(out))
    return(out)
  })
  names(ext_cols_by_pgm) <- as.character(seq_len(length(ext_cols_by_pgm)))
  return(ext_cols_by_pgm)
}
