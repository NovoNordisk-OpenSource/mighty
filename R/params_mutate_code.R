params_mutate_code <- function(.self, rename_var, source_var, node_id) {

 return(list(
    header_upper = toupper(node_id),
    self = .self,
    rename_var_upper = toupper(rename_var),
    source_var_upper = toupper(source_var)
  ))
}
