#' Fn AB
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self USUBJID
#' @outputs A
#' @outputs B
#' @returns `.self`
fn_AB <- function(.self, params = list(param_1=NULL, param_2 = NULL)){
  print("hello world")
}


#' Fn C
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self USUBJID
#' @outputs C
#' @returns `.self`
fn_C <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}


#' Fn D
#' @param .self `data.frame` Input data set
#' @type row
#' @depends .self USUBJID
#' @outputs C
#' @returns `.self`
fn_D <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}

#' Fn E
#' @param .self `data.frame` Input data set
#' @type row
#' @depends .self USUBJID
#' @outputs C
#' @returns `.self`
fn_E <- function(.self, params = list(param_1=NULL)){
  print("hello world")
}
