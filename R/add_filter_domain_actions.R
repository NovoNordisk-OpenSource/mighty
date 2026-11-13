#' Add Filter Domain Actions
#'
#' @description
#' Creates filter_domain actions that apply row-level population filters defined
#' in the YAML spec's `population` section (both `base` and `global` filters).
#'
#' Filter conditions can reference columns from external domains (e.g.,
#' `ADSL.SAFFL` when filtering ADLB). This function ensures the required join
#' keys are included as dependencies so external columns can be looked up at
#' runtime.
#'
#' @details
#' This is separate from init_domain because filtering may require joins to
#' external datasets that aren't available during initialization. The
#' init_domain action row-binds the base datasets, while filter_domain joins
#' any external datasets needed for filter conditions, then applies the filters.
#'
#'
#' For each domain with filter dependencies, this function:
#' 1. Parses external domain references (e.g., "ADSL.SAFFL" -> domain "ADSL")
#' 2. Looks up join keys for external domains from trial metadata
#' 3. Builds a dependency list combining filter columns and join keys
#' 4. Finds the column actions the filter needs, which must run before it
#' 5. Creates a filter_domain action with appropriate inputs/outputs
#'
#' Throws an error if external domains are not found in the domain_keys lookup.
#'
#' @param actions Data table containing existing action definitions with columns
#'   including node_id, type, domain, outputs, and depend_cols.
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @param filter_domain Named list of filter domain specifications per domain.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   respective key columns required for joining operations.
#'
#' @return
#' Data table combining the original actions with newly created filter_domain
#' actions, maintaining the same structure and column definitions.
#' @noRd
add_filter_domain_actions <- function(
  actions,
  filter_depend_cols,
  filter_domain,
  domain_keys
) {
  # Input validation

  checkmate::assert_data_table(actions)
  checkmate::assert_names(
    names(actions),
    must.include = c("node_id", "type", "domain", "outputs", "depend_cols")
  )
  checkmate::assert_list(filter_depend_cols, names = "named")
  checkmate::assert_list(filter_domain, names = "named")
  checkmate::assert_set_equal(names(filter_depend_cols), names(filter_domain))
  checkmate::assert_list(domain_keys, names = "named")

  domains_w_filter <- get_domains_with_filters(filter_depend_cols)

  # Extract external domains from filter dependencies
  filter_ext_domains <- parse_external_domain_refs(
    filter_depend_cols,
    domains_w_filter
  )

  filter_cols <- lapply(domains_w_filter, function(domain) {
    get_filter_column_sets(
      target_domain = domain,
      filter_depends = filter_depend_cols[[domain]],
      ext_domains = filter_ext_domains[[domain]],
      base_domains = get_base_domains(filter_domain[[domain]]),
      domain_keys = domain_keys
    )
  }) |>
    setNames(domains_w_filter)

  has_domain_filter <- has_domain_level_filter(filter_domain)

  # Create filter_domain actions
  domain_filter_actions <- lapply(domains_w_filter, function(domain) {
    create_filter_domain_action(
      target_domain = domain,
      actions = actions,
      filter_cols = filter_cols[[domain]],
      has_domain_filter = has_domain_filter[[domain]]
    )
  }) |>
    rbindlist()

  assert_no_filter_name_clash(
    domain_filter_actions,
    actions,
    filter_depend_cols,
    filter_domain,
    domain_keys
  )

  # Add domain_filter actions to existing actions
  rbind(actions, domain_filter_actions)
}

#' Find domains that have filter dependencies
#'
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @return Character vector of domain names that have filters.
#' @noRd
get_domains_with_filters <- function(filter_depend_cols) {
  has_filter <- vapply(
    filter_depend_cols,
    \(x) any(!is.na(x)),
    logical(1)
  )
  names(which(has_filter))
}

#' Get the starting domains of a filter domain specification
#'
#' @param domain_filter Filter domain specification of one domain, i.e. one
#'   element of `filter_domain`.
#' @return Character vector of domain names, `NULL` if there are none.
#' @noRd
get_base_domains <- function(domain_filter) {
  names(unlist(domain_filter, recursive = FALSE))
}

#' Get the column sets that describe what a filter needs
#'
#' Column names in the sets are upper-cased, so they can be compared with
#' action columns regardless of case.
#'
#' @param target_domain Character string of the domain name.
#' @param filter_depends Character vector of filter dependencies of the domain.
#' @param ext_domains Character vector of external domain names referenced in
#'   the filter dependencies.
#' @param base_domains Character vector of the domain's starting domains.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   key columns.
#' @return List with:
#'   - `own_domains`: the domain and its starting domains. A column of one of
#'     them is the domain's own column.
#'   - `needed`: the domain's own columns the filter needs, i.e. its bare
#'     dependencies and the join keys for the joined domains.
#'   - `joined`: data.table of the columns joined in from other domains, with
#'     the prefixed dependency (`ref`) and the column name (`column`).
#'   - `join_key_refs`: named character vector of the columns in `needed` that
#'     are join keys but not filter dependencies. Names are the domain's
#'     columns, values the prefixed keys of the joined domain (e.g.
#'     `c(VISITNUM = "SV.VISITNUM")`).
#'   - `deps_w_keys`: data.table of the filter dependencies with join keys,
#'     see `build_filter_deps_with_keys()`.
#' @noRd
get_filter_column_sets <- function(
  target_domain,
  filter_depends,
  ext_domains,
  base_domains,
  domain_keys
) {
  deps_w_keys <- build_filter_deps_with_keys(
    target_domain,
    filter_depends,
    ext_domains,
    domain_keys
  )
  own_cols <- toupper(deps_w_keys[domain == target_domain, column_name])
  join_key_cols <- setdiff(own_cols, toupper(filter_depends))
  # A key can belong to several joined domains; messages name the first one
  join_keys <- deps_w_keys[
    domain != target_domain & toupper(column_name) %in% join_key_cols
  ][!duplicated(toupper(column_name))]

  list(
    own_domains = c(target_domain, base_domains),
    needed = unique(own_cols),
    joined = get_joined_filter_cols(
      filter_depends,
      target_domain,
      base_domains,
      domain_keys
    ),
    join_key_refs = setNames(
      paste0(join_keys$domain, ".", join_keys$column_name),
      toupper(join_keys$column_name)
    ),
    deps_w_keys = deps_w_keys
  )
}

#' Get the columns a filter joins in from other domains
#'
#' Prefixes that name the domain itself or one of its starting domains, and
#' join keys of the other domain, are left out: they don't add a new column.
#'
#' @param filter_depends Character vector of filter dependencies of one domain.
#' @param target_domain Character string of the domain name.
#' @param base_domains Character vector of the domain's starting domains.
#' @param domain_keys Named list mapping domain names (uppercase) to their
#'   key columns.
#' @return data.table with one row per joined column: `ref`, the prefixed
#'   dependency as written (e.g. `"SV.VISIT"`), and `column`, the upper-cased
#'   column name.
#' @noRd
get_joined_filter_cols <- function(
  filter_depends,
  target_domain,
  base_domains,
  domain_keys
) {
  prefixed <- filter_depends[has_domain_prefix(filter_depends)]
  prefixes <- toupper(extract_domain_prefix(prefixed))
  columns <- toupper(extract_dependency_id(prefixed))
  is_external <- !prefixes %in% toupper(c(target_domain, base_domains))
  is_key <- vapply(
    seq_along(prefixed),
    function(i) columns[[i]] %in% toupper(domain_keys[[prefixes[[i]]]]),
    logical(1)
  )
  unique(data.table(ref = prefixed, column = columns)[is_external & !is_key])
}

#' Parse external domain references from filter dependencies
#'
#' Extracts domain names from "domain.column" format strings.
#'
#' @param filter_depend_cols Named list of filter dependency columns per domain.
#' @param domains_w_filter Character vector of domain names that have filters.
#' @return Named list of external domain names per ADaM domain.
#' @noRd
parse_external_domain_refs <- function(filter_depend_cols, domains_w_filter) {
  filter_dependencies <- filter_depend_cols[domains_w_filter]
  lapply(filter_dependencies, function(cols) {
    domain_prefixed <- cols[has_domain_prefix(cols)]
    extract_domain_prefix(domain_prefixed) |> unique()
  })
}

#' Get join keys for external domains referenced in filters
#'
#' Returns both unprefixed and prefixed key columns for each external domain.
#' Both are needed because joining requires matching keys from both sides:
#' unprefixed keys for the target domain, prefixed keys for the external domain.
#'
#' @param ext_domains Character vector of external domain names referenced in
#'   filter conditions (e.g., "ADSL" from "ADSL.SAFFL").
#' @param domain_keys Named list mapping domain names to their key columns.
#' @param target_domain Character string of the ADaM domain being filtered.
#'   Used in error messages.
#' @return Character vector of key columns (both prefixed and unprefixed),
#'   or NULL if ext_domains is empty.
#' @noRd
get_filter_join_keys_external_domains <- function(
  ext_domains,
  domain_keys,
  target_domain
) {
  if (length(ext_domains) == 0) {
    return(NULL)
  }

  unknown_domains <- setdiff(toupper(ext_domains), names(domain_keys))
  if (length(unknown_domains) > 0) {
    n <- length(unknown_domains)
    domain_list <- format_list(unknown_domains, format_domain)
    filter_msg <- paste0(
      "Filter for ",
      format_domain(target_domain),
      " references ",
      n,
      cli::format_inline("{cli::qty(n)} unknown domain{?s}: "),
      domain_list
    )
    info_msg <- cli::format_inline(
      "{cli::qty(n)}{?This domain/These domains} must have join keys defined to be used in filters"
    )

    throw_validation_error(
      category = "Unknown domains in filter",
      details = c(
        "x" = filter_msg,
        "i" = info_msg
      ),
      suggestions = c(
        "Add key definitions to {.file _mighty.yml} {.field /external_data} for the referenced domains",
        "Verify the domain names are spelled correctly",
        "Ensure all domains used in filters are defined in your trial metadata"
      )
    )
  }

  keys <- domain_keys[toupper(ext_domains)]
  lapply(ext_domains, function(nm) {
    key_cols <- keys[[toupper(nm)]]
    c(key_cols, paste0(nm, ".", key_cols))
  }) |>
    unlist()
}

#' Build filter dependencies with join keys for one domain
#'
#' Combines filter dependency columns with join keys from external domains,
#' then converts to a data.table with domain prefixes.
#'
#' @param domain Character string of the target ADaM domain name.
#' @param domain_filter_deps Character vector of filter dependency columns.
#' @param ext_domains Character vector of external domain names referenced in
#'   filter conditions.
#' @param domain_keys Named list mapping domain names to their key columns.
#' @return data.table with columns: column_name, domain, domain_type.
#' @noRd
build_filter_deps_with_keys <- function(
  domain,
  domain_filter_deps,
  ext_domains,
  domain_keys
) {
  keys_external_domains <- get_filter_join_keys_external_domains(
    ext_domains,
    domain_keys,
    domain
  )

  # Collect keys and existing filter dependencies
  all_filter_deps <- c(
    domain_filter_deps,
    keys_external_domains
  ) |>
    unique()

  # Add prefix for self domain
  is_self <- !grepl("\\.", all_filter_deps)
  all_filter_deps[is_self] <- paste0(
    domain,
    ".",
    all_filter_deps[is_self]
  )

  dt <- all_filter_deps |>
    tstrsplit("\\.") |>
    as.data.table()

  setnames(dt, c("domain", "column_name"))
  dt[["domain_type"]] <- classify_data_domains(dt[["domain"]])
  dt
}

#' Get column actions that must run before the filter
#'
#' Finds col_compute, col_rename and col_mutate actions whose outputs the
#' filter needs, plus all their parents within the domain. Also finds
#' col_rename actions whose source column has the name of a column joined in
#' for the filter. When one of them is a col_rename, the actions that read its
#' source column are added too, with their parents, and so are col_echo
#' actions that join on it. These actions run before the filter_domain
#' action, so their outputs must be kept by it.
#'
#' @param actions Data table containing existing action definitions.
#' @param target_domain Character string of the domain name.
#' @param filter_cols List of filter column sets, see
#'   `get_filter_column_sets()`.
#' @return Character vector of node_ids of the matching actions and their parents.
#' @noRd
get_pre_filter_nodes <- function(actions, target_domain, filter_cols) {
  domain_actions <- actions[domain == target_domain]
  candidates <- domain_actions[
    type %in% c("col_compute", "col_rename", "col_mutate")
  ]
  if (nrow(candidates) == 0) {
    return(character(0))
  }

  outputs_by_node <- candidates[,
    .(output = toupper(unlist(outputs))),
    by = node_id
  ]
  needed_nodes <- outputs_by_node[
    output %in% filter_cols$needed,
    unique(node_id)
  ]

  # A col_rename whose source has the name of a joined column must run before
  # the filter, or the join gives two columns with that name
  renames_joined_name <- get_reading_nodes(
    candidates[type == "col_rename"],
    filter_cols$joined$column,
    filter_cols$own_domains
  )

  matching_nodes <- unique(c(needed_nodes, renames_joined_name))
  if (length(matching_nodes) == 0) {
    return(character(0))
  }

  pre_filter_nodes <- add_parent_nodes(matching_nodes, domain_actions)

  # A col_rename that runs before the filter removes its source column, so
  # the actions that read that column must run before the filter too. Their
  # parents can be col_renames as well, so repeat until nothing is added.
  # A col_echo reads the column as a join key; its other dependencies are in
  # another domain, so it has no parents to add here
  echoes <- domain_actions[type == "col_echo"]
  repeat {
    rename_sources <- candidates[
      type == "col_rename" & node_id %in% pre_filter_nodes
    ] |>
      get_rename_source_cols() |>
      toupper()
    source_readers <- setdiff(
      get_reading_nodes(candidates, rename_sources, filter_cols$own_domains),
      pre_filter_nodes
    )
    echo_readers <- setdiff(
      get_reading_nodes(echoes, rename_sources, filter_cols$own_domains),
      pre_filter_nodes
    )
    if (length(source_readers) == 0 && length(echo_readers) == 0) {
      break
    }
    pre_filter_nodes <- unique(c(
      pre_filter_nodes,
      add_parent_nodes(source_readers, domain_actions),
      echo_readers
    ))
  }

  pre_filter_nodes
}

#' Get the actions that read any of the given columns of the domain
#'
#' A dependency on another domain's column with the same name doesn't count.
#'
#' @param domain_actions Data table of the actions of one domain.
#' @param columns Character vector of upper-cased column names.
#' @param own_domains Character vector of the domain and its starting domains.
#' @return Character vector of node_ids.
#' @noRd
get_reading_nodes <- function(domain_actions, columns, own_domains) {
  reads <- vapply(
    domain_actions$depend_cols,
    function(dc) {
      is_own <- toupper(dc$domain) %in% toupper(own_domains)
      any(toupper(dc$column_name[is_own]) %in% columns)
    },
    logical(1)
  )
  domain_actions$node_id[reads]
}

#' Add the parents of actions within a domain
#'
#' @param nodes Character vector of node_ids.
#' @param domain_actions Data table of the actions of one domain.
#' @return Character vector of node_ids of the actions and their parents.
#' @noRd
add_parent_nodes <- function(nodes, domain_actions) {
  # get_all_parents() handles one node at a time
  parent_nodes <- lapply(
    nodes,
    get_all_parents,
    actions = domain_actions
  ) |>
    unlist()
  unique(c(nodes, parent_nodes))
}

#' Get source columns of col_rename actions
#'
#' @param rename_actions Data table of col_rename actions.
#' @return Character vector of the columns the actions rename.
#' @noRd
get_rename_source_cols <- function(rename_actions) {
  lapply(rename_actions$depend_cols, function(dc) dc$column_name) |>
    unlist() |>
    unique()
}

#' Get source columns of col_rename actions that run after the filter
#'
#' A col_rename action reads its source column from init_domain. The
#' filter_domain action must keep that column, so the rename can run after
#' the filter. Sources that are outputs of other actions are not included,
#' because those actions provide them. Neither are sources that a col_rename
#' running before the filter removes.
#'
#' @param actions Data table containing existing action definitions.
#' @param target_domain Character string of the domain name.
#' @param pre_filter_nodes Character vector of node_ids that run before the filter.
#' @return Character vector of source columns to keep in the filter.
#' @noRd
get_rename_source_filter_outputs <- function(
  actions,
  target_domain,
  pre_filter_nodes
) {
  renames <- actions[type == "col_rename" & domain == target_domain]
  post_filter_renames <- renames[!node_id %in% pre_filter_nodes]
  if (nrow(post_filter_renames) == 0) {
    return(character(0))
  }

  action_outputs <- actions[
    domain == target_domain & type != "init_domain",
    unlist(outputs)
  ]
  pre_filter_sources <- get_rename_source_cols(
    renames[node_id %in% pre_filter_nodes]
  )
  unavailable <- toupper(c(action_outputs, pre_filter_sources))
  sources <- get_rename_source_cols(post_filter_renames)
  sources[!toupper(sources) %in% unavailable]
}

#' Find the columns a filter uses both before and after their rename
#'
#' A col_rename action that runs before the filter removes its source column.
#' The filter then can't also use the source column. The col_rename runs
#' before the filter when the filter needs its output, or when the filter
#' joins a column with the source's name.
#'
#' @param actions Data table containing existing action definitions.
#' @param target_domain Character string of the domain name.
#' @param pre_filter_nodes Character vector of node_ids that run before the filter.
#' @param filter_cols List of filter column sets, see
#'   `get_filter_column_sets()`.
#' @return data.table with one row per clash: `source`, `renamed_to`, `kind`
#'   and `ref`. `kind` is `"join_key"` when the source is a join key of a
#'   joined domain (`ref` is the prefixed key), `"joined"` when the filter
#'   joins a column with the source's name (`ref` is that prefixed
#'   dependency), or `"needed"` when the filter needs the new name (`ref` is
#'   `NA`).
#' @noRd
find_filter_on_renamed_source <- function(
  actions,
  target_domain,
  pre_filter_nodes,
  filter_cols
) {
  pre_filter_renames <- actions[
    type == "col_rename" &
      domain == target_domain &
      node_id %in% pre_filter_nodes
  ]
  # One source per rename, paired with its new name, so not get_rename_source_cols()
  source <- vapply(
    pre_filter_renames$depend_cols,
    function(dc) dc$column_name[[1]],
    character(1)
  )
  renamed_to <- vapply(
    pre_filter_renames$outputs,
    function(out) out[[1]],
    character(1)
  )
  is_clash <- toupper(source) %in% filter_cols$needed
  source <- source[is_clash]
  renamed_to <- renamed_to[is_clash]

  join_key_ref <- unname(filter_cols$join_key_refs[toupper(source)])
  joined_ref <- filter_cols$joined$ref[
    match(toupper(source), filter_cols$joined$column)
  ]
  data.table(
    source = source,
    renamed_to = renamed_to,
    kind = fifelse(
      !is.na(join_key_ref),
      "join_key",
      fifelse(!is.na(joined_ref), "joined", "needed")
    ),
    ref = fifelse(is.na(join_key_ref), joined_ref, join_key_ref)
  )
}

#' Check that a filter doesn't use a column both before and after its rename
#'
#' @inheritParams find_filter_on_renamed_source
#' @return Invisible NULL. Throws a validation error if a clash is found.
#' @noRd
assert_no_filter_on_renamed_source <- function(
  actions,
  target_domain,
  pre_filter_nodes,
  filter_cols
) {
  clashes <- find_filter_on_renamed_source(
    actions,
    target_domain,
    pre_filter_nodes,
    filter_cols
  )
  if (nrow(clashes) == 0) {
    return(invisible(NULL))
  }

  details <- vapply(
    seq_len(nrow(clashes)),
    function(i) format_renamed_source_clash(target_domain, clashes[i]),
    character(1)
  )
  throw_validation_error(
    category = "Filter uses a renamed column under both names",
    details = setNames(details, rep("x", length(details))),
    suggestions = if (all(clashes$kind == "join_key")) {
      "List the source column under {.field columns} to keep it as a separate column"
    } else {
      c(
        "Use only the new name in the filter",
        "Or list the source column under {.field columns} to keep it as a separate column"
      )
    }
  )
}

#' Format one clash found by `find_filter_on_renamed_source()`
#'
#' @param target_domain Character string of the domain name.
#' @param clash One-row data.table, see `find_filter_on_renamed_source()`.
#' @return Character string.
#' @noRd
format_renamed_source_clash <- function(target_domain, clash) {
  filter <- paste0("Filter for ", format_domain(target_domain))
  source <- format_column(clash$source)
  renamed_to <- format_column(clash$renamed_to)
  switch(
    clash$kind,
    needed = paste0(
      filter,
      " uses ",
      source,
      ", but it also needs ",
      renamed_to,
      ", which is ",
      source,
      " renamed"
    ),
    joined = paste0(
      filter,
      " uses ",
      source,
      " and also joins ",
      format_column_ref(clash$ref),
      ", which has the same name, so ",
      source,
      " is renamed to ",
      renamed_to,
      " before the filter"
    ),
    join_key = paste0(
      filter,
      " joins ",
      format_domain(extract_domain_prefix(clash$ref)),
      " on ",
      source,
      ", but it also needs ",
      renamed_to,
      ", which is ",
      source,
      " renamed, so ",
      source,
      " is gone before the join"
    )
  )
}

#' Create a single filter_domain action for one domain
#'
#' Assembles a filter_domain action that declares:
#' - outputs: columns passed through from col_copy actions, sources of
#'   col_rename actions that run after the filter, and outputs of column
#'   actions the filter needs
#' - depend_cols: columns needed from init_domain plus external filter dependencies
#'
#' @param target_domain Character string of the domain name.
#' @param actions Data table containing existing action definitions.
#' @param filter_cols List of filter column sets, see
#'   `get_filter_column_sets()`.
#' @param has_domain_filter Logical indicating if domain has base-level filters
#'   (determines whether SRC_ column is included in dependencies).
#' @return Single-row data.table representing the filter_domain action.
#' @noRd
create_filter_domain_action <- function(
  target_domain,
  actions,
  filter_cols,
  has_domain_filter
) {
  pre_filter_nodes <- get_pre_filter_nodes(actions, target_domain, filter_cols)
  assert_no_filter_on_renamed_source(
    actions,
    target_domain,
    pre_filter_nodes,
    filter_cols
  )

  # Create outputs for filter_domain from three sources:
  # 1) outputs from all col_copys that are consumed by init_domain
  # 2) source columns of col_renames that run after the filter
  # 3) all outputs from column actions the filter needs, and their parents
  col_copy_outputs <- actions[
    type == "col_copy" & domain == target_domain,
    unlist(outputs)
  ]
  rename_source_outputs <- get_rename_source_filter_outputs(
    actions,
    target_domain,
    pre_filter_nodes
  )
  pre_filter_outputs <- actions[node_id %in% pre_filter_nodes, unlist(outputs)]
  filter_domain_outputs <- c(
    col_copy_outputs,
    rename_source_outputs,
    pre_filter_outputs
  )

  # Create depend_cols for filter_domain

  # This has two sources:
  # 1) Outputs inherited from domain_init, i.e. all col_copy + SRC_ (if domain filters are present)
  # 2) filter dependencies

  depend_cols_nm <-
    if (has_domain_filter) {
      c(filter_domain_outputs, "SRC_")
    } else {
      filter_domain_outputs
    }
  inherited_depends_cols <- data.table(
    column_name = depend_cols_nm,
    domain = target_domain,
    domain_type = classify_data_domains(target_domain)
  )

  # Collect the two sources of depend_cols
  filter_domain_dep_cols <- unique(rbind(
    inherited_depends_cols,
    filter_cols$deps_w_keys
  ))

  # Consolidate domain_filter action for domain
  data.table(
    node_id = paste0(target_domain, "-filter_domain"),
    code_id = "mighty_filter_domain",
    type = "filter_domain",
    depend_cols = list(filter_domain_dep_cols),
    outputs = list(filter_domain_outputs),
    depend_rows = list(NA),
    parameters = list(NA),
    domain = target_domain
  )
}
