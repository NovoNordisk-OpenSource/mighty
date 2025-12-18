#' Write ADAM Specifications to YAML Files
#'
#' @description
#' Writes a collection of ADaM specifications to individual
#' YAML files (file per domain). This is the main entry point for
#' exporting ADaM specifications from internal datamodel back to YAML format.
#'
#' @param adam_specs A named list of ADaM specifications, where each element
#'   represents a domain specification. Names should correspond to domain names
#'   (e.g., "ADSL", "ADAE", "ADVS").
#' @param output_dir Character string specifying the output directory path.
#'   Defaults to current directory (".").
#'
#' @return A list of output file paths, one for each domain specification written.
#'
#' @details
#' This function iterates through each domain in the \code{adam_specs} collection
#' and writes it to a separate YAML file named "\{domain_name\}.yml". Each
#' specification is processed through the internal-to-YAML conversion pipeline
#' to ensure proper formatting and structure.
#'
#' @examples
#' \dontrun{
#' # Write all ADAM specs to current directory
#' output_files <- write_adam_specs(my_adam_specs)
#'
#' # Write to specific directory
#' output_files <- write_adam_specs(my_adam_specs, "output/specs")
#'
#' # Example output: c("./ADSL.yml", "./ADAE.yml", "./ADVS.yml")
#' }
#'
#' @seealso
#' \code{\link{write_adam_domain_yml}} for writing individual domain specifications
#' \code{\link{convert_internal_to_yaml}} for internal structure conversion.
#' IMPORTANT: In case of changes to internal datamodel (e.g. proposal to change
#' 'source' to 'depends') the convert_internal_to_yaml must be updated.
#'
#' @noRd
write_adam_specs <- function(adam_specs, output_dir = ".") {
  lapply(names(adam_specs), function(domain_name) {
    spec <- adam_specs[[domain_name]]
    output_path <- file.path(output_dir, paste0(domain_name, ".yml"))
    write_adam_domain_yml(spec, output_path)
    return(output_path)
  })
}

#' Write Single ADaM Domain Specification to YAML File
#'
#' @description
#' Writes a single ADaM domain specification to a YAML file.
#'
#' @param spec A single ADAM domain specification object (list) containing
#'   the internal representation of column metadata, row actions, and other
#'   domain-specific information.
#' @param output_path Character string specifying the full path for the output
#'   YAML file, including filename and extension.
#'
#' @return Character string of the output file path.
#'
#' @examples
#' \dontrun{
#' # Write single domain spec
#' output_path <- write_adam_domain_yml(adsl_spec, "ADSL.yml")
#'
#' # Write to specific directory
#' output_path <- write_adam_domain_yml(adae_spec, "specs/ADAE.yml")
#'}
#'
#' @noRd
write_adam_domain_yml <- function(spec, output_path) {
  # Convert internal structure back to YAML structure
  yaml_structure <- convert_internal_to_yaml(spec)

  # Write to YAML file
  yaml::write_yaml(yaml_structure, output_path)

  return(output_path)
}

#' Convert internal data model structure to ADaM YAML specification
#'
#' @description
#' Converts an internal specification object back to the "UI
#' YAML" structure format. This function reverses the transformations applied
#' during YAML reading to restore the original specification format (see
#' \code{\link{read_adam_domain_yml}}.
#'
#' @param spec A single ADAM domain specification in internal format.
#'
#' @return A list representing the YAML structure with the following components:
#' \describe{
#'   \item{table}{ADaM domain}
#'   \item{init}{List containing initialization settings including core_domains
#'     and filter specifications}
#'   \item{column_action}{List of column specifications}
#'   \item{row_action}{List of row-level operations (if present)}
#' }
#'
#' @details
#' The conversion process includes:
#' \itemize{
#'   \item Converting \code{NA_character_} values back to "NA" strings
#'   \item Separating column metadata from row actions: In internal data model,
#'     row actions are not named items and can be separated based on this.
#'   \item Reversing field name transformations:
#'     \itemize{
#'       \item \code{depend_cols} to \code{source}
#'       \item \code{outputs} to \code{column}
#'     }
#'   \item Removing \code{NA} values from \code{depend_rows} and \code{parameters}
#'     fields that were added during reading
#' }
#'
#' @noRd
convert_internal_to_yaml <- function(spec) {
  # Convert NA_character back to "NA" strings
  spec_with_na_strings <- convert_from_NA_character(spec)
  # Separate column_action from row_action
  columns_list <- spec_with_na_strings$columns
  column_action <- list()
  row_action <- list()
  for (i in seq_along(names(columns_list))) {
    item <- columns_list[[i]]
    item_name <- names(columns_list)[i]
    # Reverse the transformations from read_adam_domain_yml
    reversed_item <- list()

    # Copy other elements, excluding the internal ones
    for (field in names(item)) {
      if (field == "depend_cols") {
        reversed_item[["source"]] <- item[[field]]
      } else if (field == "outputs") {
        reversed_item[["column"]] <- item[[field]]
      } else {
        # While reading yaml specs, NA fields were added to depend_rows and
        # parameters if elements were not present. Ensure these are not restored
        if (
          !any(is.na(item[[field]])) ||
            field %in% c("depend_rows", "parameters")
        ) {
          if (!(
              field %in% c("depend_rows", "parameters") &&
              any(is.na(item[[field]]))
          )) {
            reversed_item[[field]] <- item[[field]]
          }
        } else {
          reversed_item[[field]] <- item[[field]]
        }
      }
    }

    # Determine if this is column_action or row_action. Row_action does not
    # have an item_name
    if (item_name == "") {
      row_action[[length(row_action) + 1]] <- reversed_item
    } else {
      column_action[[length(column_action) + 1]] <- reversed_item
    }
  }

  # Build the final YAML structure
  yaml_structure <- list(
    table = list(
      name = spec_with_na_strings$domain
    ),
    init = spec_with_na_strings$init,
    column_action = column_action
  )
  if (length(row_action) > 0) {
    yaml_structure$row_action <- row_action
  }

  return(yaml_structure)
}

convert_from_NA_character <- function(x) {
  # Check if x is a list
  if (is.list(x)) {
    # Apply recursively
    return(lapply(x, convert_from_NA_character))
  }

  # Convert NA_character back to "NA" strings
  if (is.character(x) && any(is.na(x))) {
    x[is.na(x)] <- "NA"
  }

  # Return the possibly modified x
  return(x)
}
