#' Assert filter joins don't clash with column names
#'
#' @description
#' A prefixed filter dependency (e.g. `SV.VISIT`) is joined onto the dataset
#' before the filter runs. If the dataset already has `VISIT` then, or `VISIT`
#' is joined in from two datasets, the join gives `VISIT.x` and `VISIT.y` and
#' the generated program fails at run time.
#'
#' @details
#' Names are compared upper-cased. Skipped: the other dataset's join keys (the
#' join matches on them) and prefixes that name the dataset itself or one of
#' its starting datasets (they fail elsewhere).
#'
#' @param filter_actions Data table of filter_domain actions, one per domain.
#' @param actions Data table of the existing actions. Used to say where a
#'   clashing column comes from.
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @param filter_domain Named list of filter domain specifications per domain.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   key columns.
#'
#' @return Invisibly returns `NULL`, or throws one validation error listing
#'   every clash.
#' @noRd
assert_no_filter_name_clash <- function(
  filter_actions,
  actions,
  filter_depend_cols,
  filter_domain,
  domain_keys
) {
  clashes <- lapply(seq_len(nrow(filter_actions)), function(i) {
    domain <- filter_actions$domain[i]
    find_filter_name_clashes(
      target_domain = domain,
      outputs = filter_actions$outputs[[i]],
      actions = actions,
      filter_depends = filter_depend_cols[[domain]],
      base_domains = names(unlist(filter_domain[[domain]], recursive = FALSE)),
      domain_keys = domain_keys
    )
  })
  own_clashes <- rbindlist(lapply(clashes, `[[`, "own"))
  external_clashes <- rbindlist(lapply(clashes, `[[`, "external"))

  if (nrow(own_clashes) == 0 && nrow(external_clashes) == 0) {
    return(invisible(NULL))
  }

  throw_validation_error(
    category = "Filter column name clash",
    details = c(
      format_own_clashes(own_clashes),
      format_external_clashes(external_clashes)
    ),
    suggestions = filter_name_clash_suggestions(own_clashes, external_clashes)
  )
}

#' Find filter name clashes for one domain
#'
#' @param target_domain Character string of the domain name.
#' @param outputs Character vector of the filter_domain action's outputs.
#' @param actions Data table of the existing actions.
#' @param filter_depends Character vector of filter dependencies.
#' @param base_domains Character vector of the domain's starting datasets.
#' @param domain_keys Named list mapping domain names to their key columns.
#' @return List of two data.tables: `own` (a prefixed dependency has the name
#'   of a column the domain has when the filter runs) and `external` (one
#'   column name joined in from several datasets).
#' @noRd
find_filter_name_clashes <- function(
  target_domain,
  outputs,
  actions,
  filter_depends,
  base_domains,
  domain_keys
) {
  prefixed <- filter_depends[has_domain_prefix(filter_depends)]
  prefixed <- prefixed[!duplicated(toupper(prefixed))]
  deps <- data.table(
    dependency = prefixed,
    prefix = extract_domain_prefix(prefixed),
    column = toupper(extract_dependency_id(prefixed))
  )
  deps <- deps[!toupper(prefix) %in% toupper(c(target_domain, base_domains))]
  deps[,
    is_key := column %in% toupper(domain_keys[[toupper(prefix[1])]]),
    by = prefix
  ]

  filter_time_columns <- get_filter_time_columns(
    target_domain,
    outputs,
    actions
  )
  own <- deps[is_key == FALSE][
    filter_time_columns,
    on = "column",
    nomatch = NULL
  ]
  copied_from <- if (length(base_domains) > 0) {
    paste0("copied from ", format_list(base_domains, format_domain))
  } else {
    "copied from the starting dataset"
  }
  own[, `:=`(
    domain = target_domain,
    source = fcase(
      type == "col_compute"                                                       ,
      paste0("built by component {.file ", code_id, "}")                          ,
      type %in% c("col_rename", "col_mutate")                                     ,
      paste0("made from ", format_column(source_column), " with {.field method}") ,
      default = copied_from
    ),
    is_copy = type == "col_copy"
  )]

  external <- deps[,
    .(
      dependencies = list(dependency),
      clash = uniqueN(toupper(prefix)) > 1 && !all(is_key)
    ),
    by = column
  ][(clash)]
  external[, `:=`(domain = target_domain, clash = NULL)]

  list(own = own, external = external)
}

#' Get the domain's own columns that exist when the filter runs
#'
#' Keeps only `filter_outputs` produced by the domain's own col_copy,
#' col_compute, col_rename or col_mutate actions. A col_rename or col_mutate
#' output is a filter output only when the action runs before the filter.
#'
#' @param target_domain Character string of the domain name.
#' @param filter_outputs Character vector of the filter_domain action's outputs.
#' @param actions Data table of actions.
#' @return data.table with one row per column: `column` (upper-cased), `type`,
#'   `code_id` and `source_column` (the column a col_rename or col_mutate
#'   action reads).
#' @noRd
get_filter_time_columns <- function(target_domain, filter_outputs, actions) {
  domain_actions <- actions[
    domain == target_domain &
      type %in% c("col_copy", "col_compute", "col_rename", "col_mutate")
  ]
  columns <- domain_actions[,
    .(
      column = toupper(unlist(outputs)),
      source_column = depend_cols[[1]]$column_name[1]
    ),
    by = .(node_id, type, code_id)
  ]
  unique(columns[column %in% toupper(filter_outputs)], by = "column")[,
    .(column, type, code_id, source_column)
  ]
}

#' Format clashes with the domain's own columns as error details
#' @noRd
format_own_clashes <- function(own_clashes) {
  if (nrow(own_clashes) == 0) {
    return(character(0))
  }
  msg <- paste0(
    "Filter for ",
    format_domain(own_clashes$domain),
    " depends on ",
    format_column_ref(own_clashes$dependency),
    ", but ",
    format_domain(own_clashes$domain),
    " already has a column ",
    format_column(own_clashes$column),
    " when the filter runs (",
    own_clashes$source,
    ")"
  )
  setNames(msg, rep("x", length(msg)))
}

#' Format one column joined in from several datasets as error details
#' @noRd
format_external_clashes <- function(external_clashes) {
  if (nrow(external_clashes) == 0) {
    return(character(0))
  }
  dependencies <- vapply(
    external_clashes$dependencies,
    format_list,
    character(1),
    markup_fn = format_column_ref
  )
  msg <- paste0(
    "Filter for ",
    format_domain(external_clashes$domain),
    " depends on ",
    dependencies,
    ", so a column named ",
    format_column(external_clashes$column),
    " is joined in more than once"
  )
  setNames(msg, rep("x", length(msg)))
}

#' Build suggestions for filter name clashes
#' @noRd
filter_name_clash_suggestions <- function(own_clashes, external_clashes) {
  suggestions <- character(0)

  if (nrow(own_clashes) > 0) {
    example <- own_clashes[1]
    suggestions <- c(
      suggestions,
      paste0(
        "To filter on the other dataset's column, the dataset's column with ",
        "the same name must not exist yet when the filter runs"
      )
    )
    if (any(own_clashes$is_copy)) {
      copy_example <- own_clashes[(is_copy)][1]
      suggestions <- c(
        suggestions,
        paste0(
          "For a copied column, copy it from the other dataset instead with ",
          "{.code method: ",
          copy_example$dependency,
          "}. The column then holds the other dataset's values, not the ",
          "starting dataset's"
        )
      )
    }
    if (any(own_clashes$type == "col_compute")) {
      suggestions <- c(
        suggestions,
        paste0(
          "For a column built by a component, the filter must not need that ",
          "component, directly or through other components"
        )
      )
    }
    if (any(own_clashes$type %in% c("col_rename", "col_mutate"))) {
      suggestions <- c(
        suggestions,
        paste0(
          "For a column made with {.field method}, the filter must not need ",
          "it, directly or through components"
        )
      )
    }
    suggestions <- c(
      suggestions,
      paste0(
        "To filter on the dataset's own column, write it without the prefix ",
        "(e.g. {.code ",
        example$column,
        "}). The two columns can hold different values, so check that this is ",
        "the column you mean"
      )
    )
  }

  if (nrow(external_clashes) > 0) {
    suggestions <- c(
      suggestions,
      paste0(
        "A filter can join a column name from only one other dataset. Keep ",
        "only one of the dependencies with the same column name"
      )
    )
  }

  suggestions
}
