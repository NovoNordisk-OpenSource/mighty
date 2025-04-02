#' Collapse origin into Type column
#' @description We don't need an origin column, all the information can be
#'   contained in a single column "type", and this simplifies downstream
#'   processing. We don't treat all "column" nodes the same anyways, we need to
#'   distinguish between pred and derived anyways.
#'
#' @param nodes
#'
#' @return a data.table with the information from `origin` moved to `type` and
#'   `origin` deleted
#' @export
#'
#' @examples
collapse_origin_type_columns <- function(nodes){
  nodes[type=="column", type := origin]
  return(nodes[,!"origin"])
}
