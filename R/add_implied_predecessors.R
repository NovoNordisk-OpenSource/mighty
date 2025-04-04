#' Add predecessors not specified by the user
#' @description ".self" columns that are consumed by derivation nodes need to be
#'   listed as the outputs of other nodes. If there is a column that is not
#'   listed as an output of another node at this stage, it is assumed to be an
#'   "implied predecessor", i.e. a predecessor that was not specified by the
#'   user, but required.
#'
#'   Implied predecessors can also include "temporary variable", aka columns
#'   that are used for derivations, but are not part of the final dataset
#'
#' @return the node data model with new rows for the implied predecessors. These
#'   new rows have type == "implied_predecessor"
#' @export
#'
#' @examples
add_implied_predecessors <- function(nodes) {
  nodes_by_domain <- split(nodes, by = "domain")
  nodes_with_implied_predecessors <- lapply(nodes_by_domain, extract_implied_predecessors_i) |>
    rbindlist()

  nodes_with_implied_predecessors

}

extract_implied_predecessors_i <- function(nodes_domain_i) {
  # User-defined predecessors are ignored because they can't consume other
  # predecessors by definition

  all_dependencies <- nodes_domain_i[, depend_cols] |>
    extract_("full_name") |>
    unlist() |>
    unique()
  domain_i <- nodes_domain_i$domain[1]
  potiential_predecessors <- grep(paste0(domain_i, "\\."), all_dependencies, value = TRUE)

  output <- nodes_domain_i[, outputs] |>
    extract_("full_name") |>
    unlist() |>
    unique()

  implied_predecessors <- setdiff(potiential_predecessors, output)
  n_preds <- length(implied_predecessors)

  # Replace "self." with the domain name
  io_data_model_complete_name <- lapply(implied_predecessors, function(x) {
    data_model_columnn(sub(paste0(domain_i, "\\."), "", x), domain_i, x)
  })

  # Create new rows for the implied predecessors
  x <- data.table::data.table(matrix(
    NA,
    nrow = n_preds,
    ncol = ncol(nodes_domain_i)
  )) |>
    setnames(names(nodes_domain_i))
  x[, `:=`(domain = rep(nodes_domain_i$domain[1], n_preds),
           type = rep("implied_predecessor", n_preds),
           node_id = implied_predecessors,
           depend_cols = io_data_model_complete_name,
           outputs = io_data_model_complete_name)]

  # Add them back to the nodes
  rbindlist(list(nodes_domain_i, x))
}
