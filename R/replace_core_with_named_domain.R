replace_core_with_named_domain <- function(x, ui_init) {

  for (i in seq_len(nrow(x))) {

    if (x[["type"]][[i]] %in% c("preprocess_domain", "col_supp")){
      # For preprocess_domain and col_supp actions:
      # Replace 'core' domain and domain_type with actual core domain(s) and
      # association domain, respectively

      domain_i <- x[["domain"]][[i]]
      base_domains <- ui_init[[domain_i]][["base_domains"]]
      dep_cols_i <- x[["depend_cols"]][[i]]

      is_core_dep <- tolower(dep_cols_i$domain) == "core"

      updated_dep_cols <- expand.grid(
        "column_name" = dep_cols_i$column_name[is_core_dep],
        "domain" = base_domains,
        stringsAsFactors = FALSE
      )
      updated_dep_cols[["domain_type"]] <-
        classify_data_domains(updated_dep_cols[["domain"]])

      x$depend_cols[[i]] <- rbind(dep_cols_i[!is_core_dep,],
                                  updated_dep_cols) |>
        data.table::as.data.table()

    } else {
      # For other actions:
      # Replace 'core' domain and domain_type with actual ADaM domain and
      # 'init', respectively

      domain_i <- x[["domain"]][[i]]
      domain_type_i <- classify_data_domains(domain_i)
      dep_cols_i <- x$depend_cols[[i]]
      is_core_dep <- tolower(dep_cols_i$domain) == "core"

      dep_cols_i[is_core_dep,
                 `:=`(domain = domain_i,
                      domain_type = "init")]

      x$depend_cols[[i]] <- dep_cols_i

    }
  }
  return(x)
}
