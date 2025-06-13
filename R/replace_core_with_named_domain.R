replace_core_with_named_domain <- function(x, ui_init) {

  for (i in seq_len(nrow(x))) {

    # Extract the dependency columns and domain for the current node


    if (x[["type"]][[i]] == "domain_init"){

      domain_i <- x[["domain"]][[i]]
      core_domains <- ui_init[[domain_i]][["core_domains"]]
      dep_cols_i <- x[["depend_cols"]][[i]]

      updated_dep_cols <- expand.grid(
        "column_name" = dep_cols_i$column_name,
        "domain" = core_domains,
        stringsAsFactors = FALSE
      )
      updated_dep_cols[["domain_type"]] <-
        classify_data_domains(updated_dep_cols[["domain"]])

      x$depend_cols[[i]] <- updated_dep_cols |> data.table::as.data.table()

    } else {

      domain_i <- x[["domain"]][[i]]
      domain_type_i <- classify_data_domains(domain_i)
      dep_cols_i <- x$depend_cols[[i]]
      is_core_dep <-  tolower(dep_cols_i$domain) == "core"

      dep_cols_i[is_core_dep,
                 `:=`(domain = domain_i,
                      domain_type = "init")]

      x$depend_cols[[i]] <- dep_cols_i

    }
  }
  return(x)
}
