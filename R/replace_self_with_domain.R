#' Add doamin information to any depends values that have ".self"
#' @description
#' This is needed so the dependencies between derivations in different tables
#' can be parsed
#'
#' @return
#' @export
#'
#' @examples
replace_self_with_domain <- function(nodes){

  purrr::imap(nodes, replace_self_with_domain_i)

}

replace_self_with_domain_i <- function(domain_element, domain_name){

  match_and_replace_self <- function(action, domain_name, type=c("depend_cols", "outputs")){

    type <- match.arg(type)
    depends_values <- action[[type]]
    new_values <- paste0(domain_name, ".")
    new_text <- gsub("^self\\.", new_values, depends_values)
    action[[type]] <- new_text
    return(action)
  }

  domain_element <- lapply(domain_element,
                           match_and_replace_self,
                           domain_name,
                           type = "depend_cols")
  domain_element <- lapply(domain_element,
                           match_and_replace_self,
                           domain_name,
                           type = "outputs")


  domain_element
}


