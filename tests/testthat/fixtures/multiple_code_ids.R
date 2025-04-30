#' fn_AB
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#' outputs:
#'   - A
#'   - B
#' type: derivation
#' ```
#'
fn_AB <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}


#' fn_C
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#' outputs:
#'   - C
#' type: derivation
#' ```
#'
fn_C <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}


#' fn_D
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#' outputs:
#'   - C
#' type: derivation
#' ```
#'
fn_D <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}

#' fn_E
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - USUBJID
#' outputs:
#'   - C
#' type: derivation
#' ```
#'
fn_E <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}
