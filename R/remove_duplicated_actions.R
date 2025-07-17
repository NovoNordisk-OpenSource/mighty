remove_duplicated_actions <- function(actions){
  
actions[, depend_cols_tmp := vapply(depend_cols, paste, character(1), collapse = ", ")]
actions[, outputs_tmp := vapply(outputs, paste, character(1), collapse = ", ")]

actions_out <- actions |> unique(by=c("code_id", "outputs_tmp", "depend_cols_tmp"))
actions_out$outputs_tmp <- actions_out$depend_cols_tmp <- NULL
actions_out
}