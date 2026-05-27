#' Build parameters for the `_filter_domain` template
#'
#' @param .self Character. The name of the ADaM domain being filtered.
#' @param init_metadata List. Metadata produced by domain initialization,
#'   containing `filter_domain`, `filter_global`, and `filter_depend_cols`.
#' @param keep_vars Character vector. Columns to retain after filtering.
#'   `SRC_` is excluded automatically.
#' @param domain_keys Named list. Primary keys for each domain, used to build
#'   join `by` arguments.
#'
#' @return A named list with `self`, `joins`, `domain_filter`, `global_filter`,
#'   `keep_vars`. `domain_filter`, `global_filter`, and `keep_vars` are `NULL`
#'   when absent — whisker treats `NULL` as falsy so those blocks are suppressed.
#' @noRd
params_domain_filter_code <- function(
  .self,
  init_metadata,
  keep_vars,
  domain_keys
) {
  domain_filter_ <- init_metadata$filter_domain |> unlist(recursive = FALSE)
  global_filter <- init_metadata$filter_global
  domain_required_for_filter <- init_metadata$filter_depend_cols |>
    format_filter_depends() |>
    names()
  keys_by_domain <- domain_keys[toupper(domain_required_for_filter)]
  filter_cols <- init_metadata$filter_depend_cols |>
    format_filter_depends()

  # Some filters require joining variables from "external" datasets.
  # It is easier to pre-process this in R rather than in the whisker template
  requires_data_joins <- !is.null(domain_required_for_filter) &&
    length(domain_required_for_filter) > 0
  joins <- list()
  if (requires_data_joins) {
    joins <- lapply(domain_required_for_filter, function(i) {
      list(
        table = i,
        select_expr = c(filter_cols[i], keys_by_domain[toupper(i)]) |>
          unlist() |>
          unname() |>
          paste(collapse = ", "),
        keys = paste0(
          '"',
          keys_by_domain[toupper(i)] |> unlist(),
          '"',
          collapse = ", "
        )
      )
    })
  }

  has_domain_filter <- !is.null(domain_filter_) && any(!is.na(domain_filter_))

  # Logic for domain-specific variables
  domain_filter <- if (has_domain_filter) {
    purrr::imap_chr(
      domain_filter_,
      build_domain_specific_filter_expr
    ) |>
      paste(collapse = " | ")
  } else {
    NULL
  }

  global_filter <- if (!is.null(global_filter) && any(!is.na(global_filter))) {
    global_filter
  } else {
    NULL
  }

  # SRC_ is only used for domain filters, no longer relevant for the keep step
  keep_vars <- setdiff(keep_vars, "SRC_")
  keep_vars <- if (length(keep_vars) > 0) paste(keep_vars, collapse = ", ") else NULL

  return(list(
    self = .self,
    joins = joins,
    domain_filter = domain_filter,
    global_filter = global_filter,
    keep_vars = keep_vars
  ))
}


#' Split domain-prefixed filter dependency strings into a named list
#'
#' @param x Character vector of dependency strings, some prefixed with a domain
#'   (e.g. `"ADSL.SEX"`).
#' @return Named list keyed by domain, where each element is the column names
#'   required from that domain. Non-prefixed entries are dropped.
#' @noRd
format_filter_depends <- function(x) {
  domain_prefixed <- x[has_domain_prefix(x)]
  if (length(domain_prefixed) == 0) {
    return(list())
  }
  domains <- extract_domain_prefix(domain_prefixed)
  variables <- extract_dependency_id(domain_prefixed)
  split(variables, domains)
}

#' Build a single domain-specific filter expression for use in `_filter_domain`
#'
#' @param filter_condition Character or `NA`. The per-domain filter from the
#'   YAML spec. `NA` means "keep all rows from this domain".
#' @param domain_name Character. The source domain name (e.g. `"LB"`).
#'
#' @return A character string such as `"(SRC_ == 'LB')"` or
#'   `"(SRC_ == 'XL' & LBCAT == 'CHEMISTRY')"`.
#' @noRd
build_domain_specific_filter_expr <- function(
  filter_condition,
  domain_name
) {
  base_condition <- paste0("(SRC_ == '", domain_name, "'")

  if (is.na(filter_condition)) {
    paste0(base_condition, ")")
  } else {
    paste0(base_condition, " & ", filter_condition, ")")
  }
}
