#' Write the R code to initialize a domain
#'
#' @param .self Character of length 1. Name of domain
#' @param core_domains list of core SDTM domains used to make "base" table
#' @param filter_expr list of stings with lenght equal to number of core_domains
#'   filters to apply to each core SDTM table before combining
#' @param filter_global String. Global filter to be applied after merging
#' @param keep_vars Vector of strings naming variables from SDTM core domains
#'   kept
#'
#' @return
#' @export
#'
#' @examples
generate_initialize_domain <-  function(.self,
                                        core_domains,
                                        domain_filter = NULL,
                                        filter_global = NULL,
                                        keep_vars = NULL) {

  # Block header
  self <- toupper(.self)
  metadata_block_core <- glue::glue(
    "# Core {self} table ------------------------------"
  )

  # Read in the core domains
  filter_prepart <- function(domain, domain_filter = NULL) {

    domain_var <- domain

    if (is.null(domain_filter) || any(is.na(domain_filter))) {
      return(glue::glue("{domain_var}_tmp <- {domain_var}"))
    }
    domain_filter_collapsed <- paste(domain_filter, collapse = " &&\n")
    glue::glue("{domain_var}_tmp <- {domain_var} |> dplyr::filter({domain_filter_collapsed})")
  }

  stopifnot("Domain filters must be set to NA if not used" =
              length(domain_filter) == length(core_domains))

  prepart_exprs <- unlist(purrr::map2(core_domains, domain_filter, filter_prepart))

  # Generate row_bind expression if there are multiple core domains
  if (length(core_domains) > 1) {
    bind_expr <-  glue::glue(
      "{.self} <- rbind({paste(paste0(core_domains, '_tmp'), collapse = ', ')}) |> dplyr::as_tibble()
      rm({paste(paste0(core_domains, '_tmp'), collapse=', ')})"
    )
  } else {
    bind_expr <-  glue::glue("{.self} <- {paste0(core_domains, '_tmp')} |> dplyr::as_tibble()
                             rm({paste0(core_domains, '_tmp')})")
  }

  # When the domain is NOT ADSL, we automatically merge it one in case ADSL vars
  # are needed for global filtering.
  # TODO: This could be done smarter
  merge_expr <- if (.self != "adsl") {
    glue::glue("{.self} <- dplyr::left_join({.self}, adsl, by = c('STUDYID', 'USUBJID'))")
  } else {
    NULL
  }
  # Prepare filter_global conditionally
  filter_global_val <- if (!is.null(filter_global) &&
                           all(!is.na(filter_global)) &&
                           is.character(filter_global)) {
    stopifnot("No empty strings allowed" = all(nchar(filter_global) > 0))
    filter_global_collapsed <- paste(filter_global, collapse = " &\n")
    glue::glue("{.self} <- {.self} |> admiral::convert_blanks_to_na() |> dplyr::filter({filter_global_collapsed})")
  } else {
    NULL
  }

  select_expr <- if (!is.null(keep_vars)) {
    glue::glue("# Select domain specific predecessors
               {.self} <- dplyr::select({.self}, {toupper(paste(unique(keep_vars), collapse = ', '))})")
  } else {
    NULL
  }


  all_exprs <- c(
    metadata_block_core,
    prepart_exprs,
    bind_expr,
    merge_expr,
    filter_global_val,
    select_expr
  )

  # Remove NULL expressions from the list
  all_exprs <- all_exprs[!vapply(all_exprs, is.null, logical(1L))]

  combined_text <- paste(all_exprs, collapse = "\n\n")

  return(combined_text)
}
