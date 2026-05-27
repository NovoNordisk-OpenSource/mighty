#' Build parameters for `_col_mutate` and `_col_rename` templates
#'
#' Both `_col_mutate.mustache` and `_col_rename.mustache` share the same three
#' parameters, so this single function serves both. The templates differ only in
#' the generated R verb: `_col_mutate` uses `dplyr::mutate()` (keeps the source
#' column) while `_col_rename` uses `dplyr::rename()` (replaces it in-place).
#'
#' @param .self Character. The name of the dataset being modified.
#' @param rename_var Character. The target column name (will be uppercased).
#' @param source_var Character. The source column name (will be uppercased).
#'
#' @return A named list with `self`, `rename_var`, `source_var`.
#' @noRd
params_mutate_code <- function(.self, rename_var, source_var) {
  return(list(
    self = .self,
    rename_var = toupper(rename_var),
    source_var = toupper(source_var)
  ))
}
