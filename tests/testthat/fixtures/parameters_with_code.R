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

fn_AB <- function(.self,
                  params = list(
                    param_1 = 50,
                    param_2 = "Default string",
                    param_3 = min(99, 999),
                    param_4 = NULL,
                    param_5 = min(6,7)
                  )) {
  sum(params$param_5)
  .self <- params
  return(.self)

}


#' fn_no_params

#' @section metadata:
#' ```yaml
#' depend_cols:
#'    - USUBJID
#' outputs:
#'    - C
#' type: derivation
#' ```
#'

fn_no_params <- function(.self, b) {
  .self <- c(.self)
  return(.self)

}

#' fn_mixed_defaults_and_user_params
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'    - USUBJID
#' outputs:
#'    - D
#' type: derivation
#' ```
#'
fn_mixed_defaults_and_user_params <- function(.self,
                                              params = list(param_defualt = 5, param_user = "This should be overwritten")
){

  .self <- c(.self, params)
  return(.self)
}
