assert_all_parents_present <- function(x, check_external_adam, ui_init, domain_keys){

  # Split by ADaM domain
  x_by_domain <- split(x, by = "domain")

  # Find ADSL column dependencies per ADaM domain filter
  domains <- names(ui_init)
  adsl_filter_dep_by_domain <- lapply(domains, function(nm){
    filter_depend_cols <- ui_init[[nm]]$filter_depend_cols
    filter_depend_cols_adsl <- filter_depend_cols[grepl("^AD", filter_depend_cols, ignore.case = TRUE)]
    if (length(filter_depend_cols_adsl) > 0) {
      adsl_name <- strsplit(filter_depend_cols_adsl[[1]], "\\.")[[1]][[1]]
      filter_depend_cols_adsl <- c(filter_depend_cols_adsl,
                                   paste0(adsl_name, ".", domain_keys[["ADSL"]])) |> unique()

    }
    filter_depend_cols_adsl
  })
  names(adsl_filter_dep_by_domain) <- domains

  # Identify column dependencies per ADaM domain from two sources:
  #   - ADSL in domain filter
  #   - actions within the domain
  adam_dep_by_domain <- lapply(x_by_domain, function(z){

    # Find ADSL column dependencies in domain filter
    filter_depend_cols_adsl <- adsl_filter_dep_by_domain[[z$domain[[1]]]]

    # Find column dependencies in actions
    depend_cols <- purrr::map2(z$domain, z$depend_cols, function(domain_table, depend_col){
      from_adam_domains <- c()
      has_adam_dep <- depend_col[grepl("^AD", domain, ignore.case = TRUE), nrow(.SD)] > 0
      if(has_adam_dep){
        from_adam_domains <- depend_col[grepl("^AD", domain, ignore.case = TRUE), paste0(domain, ".", column_name)]
      }
      from_adam_domains
    }) |> unlist()

    # Combine the two sources and return set of unique column dependencies
    c(filter_depend_cols_adsl, depend_cols) |> unique()

  })

  # Identify outputs
  x_no_rows <- x[(is.na(type)|type!="row")]
  outputs <- purrr::map2(x_no_rows$domain, x_no_rows$outputs, function(domain, output){
      paste0(domain, ".", unlist(output))
    }) |> unlist()

  if (check_external_adam) {

    # If check_external_adam = TRUE then check that there are no missing ADaM
    # column dependencies across domains
    all_adam_dep <- adam_dep_by_domain |> unlist() |> as.character()
    missing_deps <- setdiff(all_adam_dep, outputs)

    if (length(missing_deps) > 0) {

      # Prepare error message
      idx <- lapply(x$depend_cols, function(y) {
        any(paste0(y$domain, ".", y$column_name) %in% missing_deps)
      }) |> unlist()
      if(any(idx)){
        actions_missing_deps_dt <- x[idx, c("domain", "column")]
        actions_missing_deps <- paste0(actions_missing_deps_dt$domain,
                                        ".",
                                       actions_missing_deps_dt$column)
      }else{
        actions_missing_deps <- NULL
      }
      filter_missing_deps <- lapply(domains, function(nm){
        if(any(adsl_filter_dep_by_domain[[nm]] %in% missing_deps)){
          paste0(nm, " filter")
        }
      }) |> unlist()
      outputs_missing_deps <- c(actions_missing_deps,
                                     filter_missing_deps)

      # Print error message
      stop(
        "\n\nThe following columns are missing in the ADaM spec:\n\t",
        paste0(sort(missing_deps), collapse = "\n\t"),
        "\nto execute:\n\t",
        paste0(sort(outputs_missing_deps), collapse = "\n\t")
      )
    }
  } else {

    # Otherwise only check that the are no missing internal parents per ADaM domain
    for(nm in domains) {

      missing_deps <- setdiff(adam_dep_by_domain[[nm]], outputs)

      missing_internal_parent_cols <- lapply(missing_deps, function(mp) {
        str_split <- strsplit(mp, "\\.")[[1]]
        if (str_split[1] == nm) {
          str_split[2]
        }
      }) |> unlist()

      if (length(missing_internal_parent_cols) > 0) {

        # Prepare error message
        idx <- lapply(x_by_domain[[nm]]$depend_cols, function(y) {
          any(y$column_name %in% missing_internal_parent_cols & y$domain == nm)
        }) |> unlist()
        actions_missing_deps <- x_by_domain[[nm]]$column[idx] |> unlist()

        # Print error message
        stop(
          "\n\nThe following columns are missing in the ADaM spec for ", nm,":\n\t",
          paste0(nm, ".", sort(missing_internal_parent_cols), collapse = "\n\t"),
          "\nto execute:\n\t",
          paste0(nm, ".", sort(actions_missing_deps), collapse = "\n\t")
        )
      }
    }
  }
}


