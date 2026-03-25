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
  # Only derivation types have outputs defined in both YAML and code
  components_to_check <- x[
    type_from_code %in% c("derivation"),
    .(domain, code_id, outputs, outputs_from_code, parameters)
  ]

  if (nrow(components_to_check) == 0) {
    return(invisible(TRUE))
  }

  # Group by code_id + parameters to handle component reuse: the same component
  # can be used multiple times with different parameter values, and each unique
  # combination should be validated independently
  components_to_check[,
    parameters_str := vapply(
      parameters,
      \(p) paste(p, collapse = ","),
      character(1)
    )
  ]

  components_grouped <- components_to_check[,
    .(
      domain = domain[1], # Preserve domain for error reporting
      yaml_outputs = list(unlist(outputs)),
      code_outputs = list(unique(unlist(outputs_from_code)))
    ),
    by = .(code_id, parameters_str)
  ]

  components_grouped[,
    outputs_match := purrr::map2_lgl(
      yaml_outputs,
      code_outputs,
      \(yaml_out, code_out) {
        extra_in_code <- setdiff(code_out, yaml_out)
        length(extra_in_code) == 0
      }
    )
  ]

  if (all(components_grouped$outputs_match)) {
    return(invisible(TRUE))
  }

  mismatches <- components_grouped[outputs_match == FALSE]

  # Build error message lines
  error_lines <- character()

  for (i in seq_len(nrow(mismatches))) {
    domain <- mismatches$domain[i]
    code_id <- mismatches$code_id[i]
    code_out <- mismatches$code_outputs[[i]]
    yaml_out <- mismatches$yaml_outputs[[i]]

    # code_id is either a file path if it's a custom component, or a plain name
    display_id <- if (grepl("/", code_id)) basename(code_id) else code_id

    code_str <- format_list(toupper(code_out), format_column)
    yaml_str <- format_list(toupper(yaml_out), format_column)
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
