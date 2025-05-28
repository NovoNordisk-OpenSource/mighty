#' Fn AB
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self USUBJID
#' @depends LB USUBJID
#' @outputs A
#' @outputs B
#' @returns `.self`
#' @export
fn_AB <- function(.self,
                  params = list(
                    param_1 = 50,
                    param_2 = "Default string",
                    param_3 = min(99, 999),
                    param_4 = NULL,
                    param_5 = min(6,7)
                  )) {
  # Some comment
  sum(params$param_5)
  .self <- params
  return(.self)

}


#' Fn no params
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self USUBJID
#' @outputs C
#' @returns `.self`
#' @export
fn_no_params <- function(.self, b) {
  .self <- c(.self)
  return(.self)

}

#' Fn mixed defaults and user params
#' @param .self `data.frame` Input data set
#' @type derivation
#' @depends .self USUBJID
#' @outputs D
#' @returns `.self`
#' @export
fn_mixed_defaults_and_user_params <- function(.self,
                                              params = list(param_defualt = 5, param_user = "This should be overwritten")
){

  .self <- c(.self, params)
  return(.self)
}
