#' Assert code component outputs are subset of YAML-declared outputs
#' @description Validates that outputs declared in code component roxygen tags
#' are a subset of the outputs declared in the YAML specification.
#'
#' @details This function checks for consistency between the outputs declared in
#' YAML specifications and those declared by the component metadata. It only
#' validates components with type "derivation" (which become col_compute),
#' since these are the only types where outputs are defined in both places.
#'
#' The validation is unidirectional: code outputs must be a subset of YAML
#' outputs. Extra outputs in the code component (not declared in YAML) will
#' cause an error. However, extra outputs in YAML (not produced by the code)
#' will pass validation, as columns may come from other components.
#'
#' Components are grouped by code_id and parameters, since the same component
#' can be used with different parameter values.
#'
#' @param x A data.table containing code components with columns:
#'   - type_from_code: The type of the component ("derivation", "predecessor", or "row")
#'   - code_id: Unique identifier for the code component
#'   - outputs: List column containing outputs defined in YAML specs
#'   - outputs_from_code: List column containing outputs detected from code
#'   - parameters: List column containing component parameters
#'
#' @return Invisibly returns TRUE if all code outputs are subsets of YAML outputs,
#'   or throws an error with detailed information about components with extra outputs
#' @noRd
assert_code_outputs_in_yaml <- function(x) {
  components <- get_derivation_components(
    x,
    c("domain", "code_id", "outputs", "outputs_from_code")
  )

  if (nrow(components) == 0) {
    return(invisible(TRUE))
  }

  mismatches <- find_mismatching_outputs(components)

  if (nrow(mismatches) == 0) {
    return(invisible(TRUE))
  }

  # Build error message lines
  error_lines <- character()

  for (i in seq_len(nrow(mismatches))) {
    domain <- mismatches$domain[i]
    code_id <- mismatches$code_id[i]
    code_out <- mismatches$code_outputs[[i]]
    yaml_out <- mismatches$yaml_outputs[[i]]

    display_id <- display_component_id(code_id)

    code_str <- format_list(code_out, format_column)
    yaml_str <- format_list(yaml_out, format_column)
    msg <- paste0(
      format_domain(domain),
      " - ",
      paste0("{.file ", display_id, "}"),
      ": ",
      "produces ",
      code_str,
      ", YAML expects ",
      yaml_str
    )

    error_lines <- c(error_lines, "i" = msg)
  }

  throw_validation_error(
    category = "Component output mismatches",
    details = error_lines,
    suggestions = c(
      "Align the component's @outputs tag with the YAML specification",
      "Remove columns from @outputs or add them to the YAML as needed"
    )
  )
}

#' Find components with outputs not declared in YAML
#'
#' For each `(code_id, parameters_hashed)` group, identifies code outputs that are
#' not present in the YAML specification. Returns only groups with mismatches.
#'
#' @param components A `data.table` returned by [get_derivation_components()]
#'   with at least `domain`, `code_id`, `outputs`, `outputs_from_code`, and
#'   `parameters_hashed`.
#' @return A `data.table` with columns `domain`, `code_id`, `yaml_outputs`,
#'   `code_outputs`, and `extra_outputs` (list columns). Only rows with
#'   non-empty extra_outputs are returned.
#' @noRd
find_mismatching_outputs <- function(components) {
  components_grouped <- components[,
    .(
      domain = domain[1], # Preserve domain for error reporting
      yaml_outputs = list(unlist(outputs)),
      code_outputs = list(unique(unlist(outputs_from_code))),
      extra_outputs = list(setdiff(
        unique(unlist(outputs_from_code)),
        unlist(outputs)
      ))
    ),
    by = .(code_id, parameters_hashed)
  ]

  components_grouped[lengths(extra_outputs) > 0]
}
