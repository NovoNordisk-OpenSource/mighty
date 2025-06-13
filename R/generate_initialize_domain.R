#' Write the R code to initialize a domain
#'
#' @param .self Character of length 1. Name of domain
#' @param core_domains list of core SDTM domains used to make "base" table
#' @param adsl_domain_keys
#' @param filter_domain
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
                                        adsl_domain_keys,
                                        filter_domain = NULL,
                                        filter_global = NULL,
                                        filter_depend_cols = NULL,
                                        keep_vars = NULL) {
  stopifnot("Domain filters must be set to NA if not used" =
              length(filter_domain) == length(core_domains))

  # Block header
  self <- toupper(.self)
  metadata_block_core <- glue::glue("# Filter {self} ------------------------------")

  # When the domain is NOT ADSL, we automatically merge it on in case ADSL vars
  # are needed for global filtering.
  filter_adsl_cols <- filter_depend_cols[grep("^adsl\\.", filter_depend_cols, ignore.case = TRUE)]
  adsl_merge_expr <- if (self != "ADSL" && length(filter_adsl_cols) > 0) {

    keys <- paste0("\"", adsl_domain_keys, "\"", collapse = ",")
    adsl_name <- gsub("\\.[a-zA-Z]+$", "", filter_adsl_cols) |> unique()
    filter_adsl_col_str <- c(gsub("^[a-zA-Z]+\\.", "", filter_adsl_cols), adsl_domain_keys) |>
      unique() |>
      paste(collapse = ", ")

    glue::glue(
      "# Add ADSL columns for filtering
              {.self} <- {.self} |>
    dplyr::left_join({adsl_name} |>
    dplyr::select({filter_adsl_col_str}),
    by = c({keys}))"
    )
  } else {
    NULL
  }

  # Prepare domain filter conditionally
  filter_domain_unlist <- unlist(filter_domain)
  if (any(!is.na(filter_domain_unlist))) {
    filter_domain_def <- lapply(core_domains, function(x) {
      filter <- filter_domain_unlist[[x]]
      if (is.na(filter)) {
        paste0("(SRC_ == \"", x, "\")")
      } else {
        paste0("(SRC_ == \"", x, "\" & ", filter, ")")
      }
    }) |> paste0(collapse = " |\n")
    filter_domain_expr <- glue::glue(
      "# Apply domain filters
            {.self} <- {.self} |>
  dplyr::filter({filter_domain_def}) |>
  dplyr::select(-SRC_)
")
  } else {
    filter_domain_expr <- NULL
  }

  # Prepare global filter conditionally
  filter_global_expr <- if (!is.null(filter_global) &&
                            all(!is.na(filter_global)) &&
                            is.character(filter_global)) {
    stopifnot("No empty strings allowed" = all(nchar(filter_global) > 0))
    filter_global_collapsed <- paste(filter_global, collapse = " &\n")
    glue::glue(
      "# Apply global filter
               {.self} <- {.self} |>
          dplyr::filter({filter_global_collapsed})"
    )
  } else {
    NULL
  }

  # Keep only predecessor columns from core domain(s)
  select_expr <- if (!is.null(keep_vars)) {
    glue::glue(
      "# Select {toupper(.self)} predecessors
               {.self} <- {.self} |>
          dplyr::select({paste(unique(keep_vars), collapse = ', ')})"
    )
  } else {
    NULL
  }

  # Collect code pieces
  all_exprs <- c(
    metadata_block_core,
    adsl_merge_expr,
    filter_domain_expr,
    filter_global_expr,
    select_expr
  )
  combined_text <- paste(all_exprs, collapse = "\n\n")

  return(combined_text)
}
