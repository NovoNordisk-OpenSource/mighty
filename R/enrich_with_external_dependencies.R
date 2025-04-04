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

    # Special case: For domain_init, we need to add the column dependencies from the global filters
    if (i$action == "domain_init") {

      # Identify the columns dependencies
      filter_depend_cols <- init_metadata[[i$domain]][["filter_depend_cols"]]

      if (all(!is.na(filter_depend_cols))) {

        # Subset column dependencies to those from the core domain
        filter_depend_cols_self <- gsub("^self\\.", "",
                                        filter_depend_cols[grepl("^self\\.",
                                                                 filter_depend_cols)])
        dep_init <- expand.grid("domain" = unique(dep$domain),
                                "column_name" = filter_depend_cols_self,
                                stringsAsFactors = FALSE)
        dep_init[["full_name"]] <- paste(dep_init[["domain"]],
                                         dep_init[["column_name"]],
                                         sep = ".")
        dep_init[["domain_type"]] <- rep(unique(dep$domain_type), nrow(dep_init))

        # Subset column dependencies to those from the ADSL domain
        dep_init_adsl <- data.table(
          domain = "adsl",
          domain_type = "adam",
          full_name = filter_depend_cols[grepl("^adsl\\.",
                                               filter_depend_cols)])
        dep_init_adsl[["column_name"]] <- gsub("^adsl\\.", "", dep_init_adsl[["full_name"]])

        # Combine the dependencies
        dep <- rbind(dep, dep_init, dep_init_adsl) |> unique()
        dep <- dep[order(dep$full_name),]
      }
    }
    return(list(dep))
  }

  out <- copy(nodes)
  out[, external_dependencies := list(add_external_by_nodes_i(.SD)), by =
        seq_len(nrow(out))]
}
