#' Function AB
#'
#' @param .self
#' @param params
#' @section outputs
#' * A
#' * B
#'
#' @section depends_cols
#' * USUBJID
#' @section type
#' * derivation
#'
#'
#' @returns
#' @export
#'
#' @examples
fn_AB <- function(
  .self,
  params = list(
    param_1 = 50,
    param_2 = "Default string",
    param_3 = min(99, 999),
    param_4 = NULL
  )
) {
  .self <- list(params$param_1, params$param_2, params$param_3)
  return(.self)
}
