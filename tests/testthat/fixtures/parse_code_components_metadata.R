#' fn_AB
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - A
#'   - B
#' outputs:
#'   - C
#' type: col_compute
#' ```
#'
fn_AB <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}


#' fn_B
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - LB.A
#'   - B
#' outputs:
#'   - C
#'   - D
#' type: col_compute
#' ```
#'
fn_b <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}
