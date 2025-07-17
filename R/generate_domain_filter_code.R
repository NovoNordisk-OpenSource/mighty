generate_domain_filter_code <- function(
  .self,
  source_domains,
  init_metadata,
  keep_columns,
  domain_keys
) {
  domain_filter_ = init_metadata$filter_domain |> unlist(recursive = F)
  global_filter = init_metadata$filter_global
  domain_required_for_filter = init_metadata$filter_depend_cols |>
    format_filter_depends() |>
    names()
  keys_by_domain <- domain_keys[domain_required_for_filter]
  unique_domain_keys <- keys_by_domain |>
    unlist() |>
    unique()
  filter_cols <- init_metadata$filter_depend_cols |>
    format_filter_depends()

  cols_required_for_filter = c(
    unique_domain_keys,
    filter_cols |> unlist() |> unname()
  )

  # Some filters require joining variables from "external" datasets.
  # It is easiers to pre-process this in R rather than in the whisker template
  requires_data_joins <- !is.null(domain_required_for_filter) &&
    length(domain_required_for_filter) > 0
  joins <- list()
  if (requires_data_joins) {
    joins <- lapply(domain_required_for_filter, function(i) {
      list(
        table = i,
        cols = c(filter_cols[i], keys_by_domain[i]) |> unlist(),
        keys = paste0('"', keys_by_domain[i] |> unlist(), '"', collapse = ", ")
      )
    })
  }

  has_domain_filter <- !is.null(domain_filter_) && any(!is.na(domain_filter_))
  has_global_filter <- !is.null(global_filter) && any(!is.na(global_filter))
  has_keep_columns <- !is.null(keep_columns) && length(keep_columns) > 0

  # Logic for domain-specific vairables
  domain_specific_filter_expr <-if (has_domain_filter) {
    purrr::imap_chr(
      domain_filter_,
      build_domain_specific_filter_expr
    ) |>
      paste(collapse = " | ")
  }

  # SRC_ is only used for domain filters, no longer relevant for the keep step
  keep_cols <- setdiff(keep_columns, "SRC_") |>
    paste0(collapse = ", ")

  data <- list(
    self = .self,
    joins = joins,
    has_domain_filter = has_domain_filter,
    domain_filter = domain_specific_filter_expr,
    has_global_filter = has_global_filter,
    global_filter = global_filter,
    has_keep_columns = has_keep_columns,
    keep_columns = keep_cols
  )

  template <- "# Filter {{self}} ----------------------

{{#joins}}
{{self}} <- {{self}} |>
  dplyr::left_join({{table}} |> dplyr::select({{cols}}),
                   by = c({{{keys}}}))

{{/joins}}
{{#has_domain_filter}}
{{self}} <-  {{self}} |>
  dplyr::filter({{{domain_filter}}}) |>
  dplyr::select(-SRC_)

{{/has_domain_filter}}
{{#has_global_filter}}
{{self}} <-  {{self}} |>
  dplyr::filter({{{global_filter}}})

{{/has_global_filter}}
{{#has_keep_columns}}
{{self}} <-  {{self}} |>
  dplyr::select({{keep_columns}})

{{/has_keep_columns}}
"

  whisker::whisker.render(template, data)
}


format_filter_depends <- function(x) {
  has_dot <- grepl("\\.", x)
  x_with_dots <- x[has_dot]
  domains <- sub("^([^.]*)\\..*", "\\1", x_with_dots)
  variables <- sub("^[^.]*\\.", "", x_with_dots)
  split(variables, domains)
}

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

