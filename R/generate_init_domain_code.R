#' Write the R code to initialize ADaM domain in "initial" programs
#'
#' @param base_domains
#' @param domain_filters_exist
#'
#' @return
#' @export
#'
#' @examples
generate_init_domain_code <- function(.self, keep_columns, source_domains) {
  # Initialize ADaM table by row binding source domain(s) and selecting
  # predecessors from source domain(s).

  # Single template that handles both cases
  template <- "# Initialize {{self_upper}} ----------------------

{{#src_mutations}}
{{domain}} <- {{domain}} |>
  dplyr::mutate(SRC_ = \"{{domain}}\")

{{/src_mutations}}
{{self}} <-  {{source_domain_rbind}} |>
    dplyr::select({{keep_columns}}) |> 
    admiral::convert_blanks_to_na()


"

  # Data preparation for SRC_ mutations
  src_mutations <- list()
  if ("SRC_" %in% keep_columns) {
    src_mutations <- source_domains |>
      lapply(function(domain) {
        list(domain = domain)
      })
  }

  # Pre-format the rbind call in R rather than in the template
  source_domain_rbind <- source_domains[1]
  if (length(source_domains) > 1) {
    source_domain_rbind <- paste0(
      "rbind(",
      paste(source_domains, collapse = ",\n"),
      ")"
    )
  }

  data <- list(
    self = .self,
    self_upper = toupper(.self),
    keep_columns = keep_columns,
    source_domain_rbind = source_domain_rbind,
    src_mutations = src_mutations
  )

  whisker::whisker.render(template, data)
}
