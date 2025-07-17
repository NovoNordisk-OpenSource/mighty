#' Write the ADaM progrms to disk and style them
#'
#' @param programs
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
write_adam_programs <- function(programs, dir, style = FALSE){
  prog_names <- names(programs)
  purrr::imap(programs, function(prog_i, nm){
    file_name <- paste0(nm, ".R")
    prog_i <- paste0(prog_i, collapse = "\n")
    writeLines(prog_i, file.path(dir, file_name))
    if(style){
styler::style_file(file.path(dir, file_name))
    }
    
  })
  return(TRUE)
}
