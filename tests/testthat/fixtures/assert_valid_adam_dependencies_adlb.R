#' lbtest2
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - LBTEST
#' outputs:
#'   - LBTEST2
#' type: derivation
#' ```
#'
lbtest2 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(
      LBTEST2 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      ))
  return(.self)
}

#' lbtest3
#'
#' @section metadata:
#' ```yaml
#' depend_cols:
#'   - LBTEST
#' outputs:
#'   - LBTEST3
#'   - LBTEST3_FLG
#' type: derivation
#' ```
#'
lbtest3 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(
      LBTEST3 = dplyr::case_when(
        !is.na(LBTEST) ~ LBTEST,
        is.na(LBTEST) ~ "Invalid"
      )) |>
    dplyr::mutate(
      LBTEST3_FLG = dplyr::case_when(
        LBTEST3 == "Invalid" ~ "Y",
        LBTEST3 != "Invalid" ~ "Invalid"
      ))
  return(.self)
}

