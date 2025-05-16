assert_all_parents_present <- function(x, check_external_adam){

  x_by_domain <- split(x, by = "domain")
  adam_dep_by_domain <- lapply(x_by_domain, function(z){
    purrr::map2(z$domain, z$depend_cols, function(domain_table, depend_col){
      from_adam_domains <- c()
      has_adam_dep <- depend_col[grepl("^AD", domain, ignore.case = TRUE), nrow(.SD)] > 0
      if(has_adam_dep){
        from_adam_domains <- depend_col[grepl("^AD", domain, ignore.case = TRUE), paste0(domain, ".", column_name)]
      }
      from_adam_domains
    }) |> unlist()
  })

  x_no_rows <- x[(is.na(type)|type!="row")]
  outputs <- purrr::map2(x_no_rows$domain, x_no_rows$outputs, function(domain, output){
      paste0(domain, ".", unlist(output))
    }) |> unlist()

  if (check_external_adam) {

    # If check_external_adam = TRUE then check that there are no missing ADaM
    # column dependencies across domains
    all_adam_dep <- adam_dep_by_domain |> unlist() |> as.character()
    missing_parent_cols <- setdiff(all_adam_dep, outputs)

    if (length(missing_parent_cols) > 0) {

      # Prepare error message
      idx <- lapply(x$depend_cols, function(y) {
        any(paste0(y$domain, ".", y$column_name) %in% missing_parent_cols)
      }) |> unlist()
      actions_missing_deps <- x[idx, c("domain", "column")]
      actions_missing_deps2 <- paste0(actions_missing_deps$domain,
                                      ".",
                                      actions_missing_deps$column)

      # Print error message
      stop(
        "\n\nThe following columns are missing in the ADaM spec:\n\t",
        paste0(sort(missing_parent_cols), collapse = "\n\t"),
        "\nto calculate:\n\t",
        paste0(sort(actions_missing_deps2), collapse = "\n\t")
      )
    }
  } else {

    # Otherwise only check that the are no missing internal parents per ADaM domain
    for(nm in names(x_by_domain)) {

      missing_parent_cols <- setdiff(adam_dep_by_domain[[nm]], outputs)

      missing_internal_parent_cols <- lapply(missing_parent_cols, function(mp) {
        str_split <- strsplit(mp, "\\.")[[1]]
        if (str_split[1] == nm) {
          str_split[2]
        }
      }) |> unlist()

      if (length(missing_internal_parent_cols) > 0) {

        # Prepare error message
        idx <- lapply(x_by_domain[[nm]]$depend_cols, function(y) {
          any(y$column_name %in% missing_internal_parent_cols)
        }) |> unlist()
        actions_missing_deps <- x_by_domain[[nm]]$column[idx] |> unlist()

        # Print error message
        stop(
          "\n\nThe following columns are missing in the ADaM spec for ", nm,":\n\t",
          paste0(sort(missing_internal_parent_cols), collapse = "\n\t"),
          "\nto calculate:\n\t",
          paste0(sort(actions_missing_deps), collapse = "\n\t")
        )
      }
    }
  }
}


