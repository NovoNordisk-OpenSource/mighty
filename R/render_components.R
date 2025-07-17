render_components <- function(code_component_ids) {
  rendered_components <- list()
  for (i in seq_len(nrow(code_component_ids))) {
    code_id <- code_component_ids[i, code_id]
    params <- code_component_ids$parameters[i] |> unlist(recursive = FALSE)
    if(!is.list(params) && is.na(params))params <- list()
    rendered_components[[code_id]] <- mighty.standards::get_rendered_component(
      component = code_id,
      params = params
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
      type = ifelse(i$type == "derivation", "col_compute", i$type)
    )
  })
}
