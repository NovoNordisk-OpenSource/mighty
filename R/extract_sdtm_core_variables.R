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

  # Pre-filter for predecessor nodes to avoid unnecessary processing
  pred_nodes <- nodes[nodes$type == "predecessor", ]

  # Group by domain
  result <- split(pred_nodes, by = "domain")

  # Process each domain group
  result <- lapply(result, function(domain_nodes) {
    # Combine all depend_cols into one data.table
    all_cols <- data.table::rbindlist(domain_nodes$depend_cols)

    # Filter for core domain and get unique column names in one step
    unique(all_cols[domain == "core", column_name])
  })

  return(result)
}
