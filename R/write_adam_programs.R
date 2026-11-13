#' Write ADaM Programs to Disk
#'
#' Writes generated ADaM programs to individual R files and optionally
#' applies code styling.
#'
#' @param programs Named list. Each element contains program code as character
#'   vector, with names used as file names (without .R extension).
#' @param dir Character. Output directory path where R files will be written.
#' @param style Logical. Whether to apply code styling using [styler::style_file()].
#'   Default is FALSE.
#'
#' @return Logical. Returns TRUE invisibly upon successful completion.
#'
#' @details
#' For each program in the list:
#' \itemize{
#'   \item Creates filename by appending ".R" to the list element name
#'   \item Collapses code vector into single string with newline separators
#'   \item Writes code to file in specified directory
#'   \item Optionally applies styler formatting if requested
#' }
#'
#' @examples
#' programs <- list(
#'   ADSL = c("library(dplyr)", "ADSL <- DM |> select(USUBJID)"),
#'   ADAE = c("library(dplyr)", "ADAE <- AE |> filter(!is.na(AEDECOD))")
#' )
#' dir <- tempfile("adam")
#' dir.create(dir)
#'
#' # Write programs without styling
#' write_adam_programs(programs, dir = dir)
#' list.files(dir)
#'
#' # Write with styling
#' write_adam_programs(programs, dir = dir, style = TRUE)
#'
#' unlink(dir, recursive = TRUE)
#' @export
write_adam_programs <- function(programs, dir, style = FALSE) {
  prog_names <- names(programs)
  purrr::imap(programs, function(prog_i, nm) {
    file_name <- paste0(nm, ".R")
    prog_i <- paste0(prog_i, collapse = "\n")
    file_path <- file.path(dir, file_name)
    writeLines(prog_i, file_path)
    if (style) {
      styler::style_file(file_path)
    }
  })
  invisible(TRUE)
}
