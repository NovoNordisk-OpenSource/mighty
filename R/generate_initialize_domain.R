#' Write the R code to initialize ADaM domain in "initial" programs
#'
#' @param core_domains
#' @param domain_filters_exist
#'
#' @return
#' @export
#'
#' @examples
generate_initialize_domain <- function(.self, core_domains, domain_filters_exist) {

  # Initialize ADaM table by row binding source domain(s) and selecting
  # predecessors from source domain(s).
  combine_core_domains <- if (length(core_domains) > 1){

    # Add temporary column src_ to tag each source domain if domain specific
    # filters are applied
    if (domain_filters_exist) {
      core_domains <- lapply(core_domains, function(x) {
        glue::glue("{x} |> dplyr::mutate(SRC_ = '{x}')")
      }) |> unlist()
    }

    core_domains_str <- paste0(core_domains, collapse = ",\n")
    glue::glue(
      "{.self} <- rbind({core_domains_str}) |>
            admiral::convert_blanks_to_na()\n\n"
    )
  } else {
    glue::glue(
      "{.self} <- {core_domains} |>
            admiral::convert_blanks_to_na()\n\n"
    )
  }

  adam_init <- paste(c(glue::glue("# Initialize {toupper(.self)} ----------------------\n\n"),
                       combine_core_domains), collapse = "\n")
}
