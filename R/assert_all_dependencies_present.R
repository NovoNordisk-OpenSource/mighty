assert_all_dependencies_present <- function(nodes) {
  # Check that, for each output, all dependencies listed in depend_cols with
  # domain =="self" are also present as outputs

  nodes_by_domain <- split(nodes, by = "domain")
  columns_required_but_not_specified_in_ADaM_specs <-
    nodes_by_domain |>
    lapply(assert_all_dependencies_present_by_domain)

  if (columns_required_but_not_specified_in_ADaM_specs |> unlist() |> is.null()){
    return(TRUE)
  }
    stop("The following collumns are missing from their respective domains: \n",
    Filter(Negate(is.null), columns_required_but_not_specified_in_ADaM_specs) |>
      print() |>
      capture.output() |>
      paste(collapse = "\n"))
}


assert_all_dependencies_present_by_domain <- function(nodes_by_domain) {
  dependencies <- lapply(nodes_by_domain$depend_cols, function(dt) {
    dt[, .(parent_column = column_name, parent_domain = domain)]
  }) |> data.table::rbindlist()

   # All columns from "self" need to be specified in the ADaM Spec yml file. We
   # are only interested in "self" dependencies, because external dependencies
   # are handled by automatically adding a data import step later in the code
   # generator.
  required_columns <- dependencies[parent_domain == "self", parent_column]

  columns_required_but_not_specified_in_ADaM_specs <- required_columns |>
    setdiff(nodes_by_domain$outputs |> unlist())
  if (length(columns_required_but_not_specified_in_ADaM_specs) == 0)
    return(NULL)
  # names(columns_required_but_not_specified_in_ADaM_specs) <- vector_domain_for_attributes
  return(columns_required_but_not_specified_in_ADaM_specs)

}
