#' Fn AB
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self A
#' @depends .self B
#' @outputs C
#' @returns `.self` with added treatment emergent analysis flag.
fn_AB <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}


#' Fn B
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends LB A
#' @depends .self B
#' @outputs C
#' @outputs D
#' @returns `.self` with added treatment emergent analysis flag.
fn_B <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}
