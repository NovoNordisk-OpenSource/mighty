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
  # Core variables consit of the unique parent columns of all predecessor nodes
  predecessor_depend_cols <- nodes_domain_i[type=="predecessor", depend_cols] |> rbindlist()
  predecessor_depend_cols[domain == "core", column_name] |> unique()

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
