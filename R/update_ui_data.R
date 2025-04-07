#' Update UI data
#'
#' @param payload
#' @param ui_data
#'
#' @return
#' @export
#'
#' @examples
update_ui_data <- function(payload, ui_data) {
  for (name in names(payload)) {
    ui_data <- updata_ui_data_i(ui_data, payload, name)
  }
  return(ui_data)
}


updata_ui_data_i <- function(ui_data, payload, name) {
  nodes <- ui_data$nodes
  action_name <- purrr::imap(nodes, function(i, domain) {
    purrr::imap(i, function(ii, inx) {
      # All function names ins std prog should be in lower case.
      if (!is.na(ii$code_id) &&
          ii$code_id == name)
        return(c(domain,inx))
    })

  })  |> unlist()
  if(is.null(action_name)){
    return(ui_data)
  }
  list_to_modify <- ui_data$nodes[[action_name[1]]][[action_name[2]]]
  list_to_modify <- modifyList(list_to_modify, payload[[name]])
  ui_data$nodes[[action_name[1]]][[action_name[2]]] <- list_to_modify

  return(ui_data)
}



update_ui_data_2 <- function(payload, ui_data) {
  browser()

  # Find ui_data elements that have code_ids. All elements with the same code id need to be merged
  for (element in ui_data){
    if(is.null(element$code)) next
browser()

  }



  # For each ui_data element, if there is a code_id child, find the element in
  # payload whose name matches that code_id and add the depend_cols and outputs fields
  for(element in ui_data){
    if(is.null(element$code_id))next
    browser()
    if(!is.null(element$depend_cols) || !is.null()){
    element <- c(element, payload[[element$code_id]][c("depend_cols", "outputs")])
  }
}

}


updata_ui_data_ii <- function(ui_data, payload, name) {
  browser()
  action_name <- purrr::imap(ui_data, function(i, domain) {
    purrr::imap(i, function(ii, inx) {
      # All function names ins std prog should be in lower case.
      if (!is.na(ii$code_id) &&
          ii$code_id == name)
        return(c(domain,inx))
    })

  })  |> unlist()
  if(is.null(action_name)){
    return(ui_data)
  }
  list_to_modify <- ui_data[[action_name[1]]][[action_name[2]]]
  list_to_modify <- modifyList(list_to_modify, payload[[name]])
  ui_data$nodes[[action_name[1]]][[action_name[2]]] <- list_to_modify

  return(ui_data)
}
