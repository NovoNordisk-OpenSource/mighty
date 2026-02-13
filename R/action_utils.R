#' Determine which domains have domain-level filters
#'
#' @param filter_domain Named list of filter domain specifications per domain.
#' @return Named logical list indicating presence of domain filters.
#' @noRd
has_domain_level_filter <- function(filter_domain) {
  lapply(filter_domain, function(x) any(!is.na(unlist(x))))
}
