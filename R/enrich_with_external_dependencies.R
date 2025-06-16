#' Parses nodes for external dependencies, including "core" variables
#' @description
#' Finds all "external" dependencies for each "node"
#'
#' @param nodes
#' @param init_metadata
#'
#' @return a list object where the name of the list element corresponds to the program_id in `grouped_nodes`
#' @export
#'
#' @examples
enrich_with_external_dependencies <- function(nodes, init_metadata) {
  add_external_by_nodes_i <- function(i) {
    # This will include all "side-loaded" and "core" variables


    dep <- i$depend_cols[[1]][domain != i$domain]

    # Special case: For preprocess_domain, we need to add the column dependencies from the global filters
    if (i$type != "preprocess_domain") {
      return(list(dep))
    }


    # Identify the columns dependencies
    filter_depend_cols <- init_metadata[[i$domain]][["filter_depend_cols"]]
    filter_missing <- is.na(filter_depend_cols) |> any()
    if (filter_missing) {
      return(list(dep))
    }
    # Column dependencies coming from the "core" domain(s)

    filter_depend_cols_self <- gsub("^core\\.", "", filter_depend_cols[grepl("^core\\.", filter_depend_cols)])
    dep_init <- expand.grid(
      "domain" = unique(dep$domain),
      "column_name" = filter_depend_cols_self,
      stringsAsFactors = FALSE
    )

    dep_init[["domain_type"]] <- rep(unique(dep$domain_type), nrow(dep_init))

    # Column dependencies coming from ADSL
    filter_depend_cols_adsl <- gsub("^ADSL\\.", "", filter_depend_cols[grepl("^ADSL\\.", filter_depend_cols)])
    dep_init_adsl <- data.table::data.table(domain = "ADSL",
                                            domain_type = "adam",
                                            column_name = filter_depend_cols_adsl)

    # Combine the dependencies
    dep <- rbind(dep, dep_init, dep_init_adsl) |> unique()
    return(list(dep))
  }

  out <- copy(nodes)
  out[, depend_cols_ext := list(add_external_by_nodes_i(.SD)), by =
        seq_len(nrow(out))]
}
