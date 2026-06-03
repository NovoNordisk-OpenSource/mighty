render_components <- function(actions, repos = NULL) {
  code_component_ids <- actions[!is.na(code_id), ]

  rendered_components <- list()
  for (i in seq_len(nrow(code_component_ids))) {
    node_id <- code_component_ids[i, node_id]
    code_id <- code_component_ids[i, code_id]
    params <- code_component_ids$parameters[i] |> unlist(recursive = FALSE)
    if (!is.list(params) && is.na(params)) {
      params <- list()
    }
    rendered_components[[node_id]] <- mighty.component::get_rendered_component(
      component = code_id,
      params = params,
      repos = repos
    )
  }
  rendered_components
}

get_component_metadata <- function(rendered_components) {
  checkmate::assert_list(rendered_components) |>
    purrr::walk(\(x) checkmate::assert_r6(x, "mighty_component_rendered"))

  lapply(rendered_components, function(i) {
    list(
      depend_cols = i$depends,
      outputs = i$outputs,
      type = i$type
    )
  })
}
