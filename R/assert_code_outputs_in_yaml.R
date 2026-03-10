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

  # Get domain from the first row (all rows should have same domain)
  domain <- components_to_check$domain[1]

  error_msg <- c("Code components declare outputs not in YAML specification:")

  for (i in seq_len(nrow(mismatches))) {
    code_id <- mismatches$code_id[i]
    code_out <- mismatches$code_outputs[[i]]
    yaml_out <- mismatches$yaml_outputs[[i]]
    extra <- setdiff(code_out, yaml_out)

    # code_id is either a file path if it's a custom component, or a plain name
    display_id <- if (grepl("/", code_id)) basename(code_id) else code_id

    error_msg <- c(
      error_msg,
      "x" = cli::format_inline(
        "{.file {domain}} - {.code {display_id}}: YAML declares {.val {yaml_out}}, ",
        "code declares {.val {code_out}}, extra in code: {.val {extra}}"
      )
    )
  }

  cli::cli_abort(error_msg)
}
