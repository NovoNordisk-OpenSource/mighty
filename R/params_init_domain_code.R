#' Generate Parameters for ADaM Domain Initialization Code
#'
#' This function prepares template parameters for generating R code that initializes
#' ADaM domains. It handles the complexities of multi-domain binding, column
#' selection, and SRC_ variable creation for use in code generation templates.
#'
#' @param .self Character. The target ADaM domain name (e.g., "ADSL", "ADAE").
#'   Used as the variable name for the initialized data set and converted to
#'   uppercase for data set labels and references.
#' @param keep_vars Character vector. Column names to retain in the initialized
#'   ADaM domain. If `"SRC_"` is included, source domain tracking variables will
#'   be generated for each contributing SDTM domain. Formatted as a
#'   comma-separated string in the returned list.
#' @param source_domains Character vector. Names of source SDTM domains to combine
#'   (e.g., c("DM", "VS", "LB")). These domains will be row-bound together to
#'   create the initial ADaM data set structure.
#'
#' @return A named list containing template parameters for code generation:
#'   \describe{
#'     \item{self}{Character. The target ADaM domain name (same as `.self`)}
#'     \item{keep_vars}{Character string. Comma-separated column names to retain,
#'       formatted for direct use in the template.}
#'     \item{source_domain_rbind}{Character. Formatted R expression for combining
#'       source domains. Single domain returns the domain name; multiple domains
#'       return a formatted `rbind()` call}
#'     \item{src_mutations}{List. Source domain mutation specifications for SRC_
#'       variable creation. Empty list if "SRC_" not in `keep_vars`}
#'   }
#'
#' @details
#' This function serves as a parameter preparation step for ADaM domain initialization
#' templates. It handles several key aspects of the initialization process:
#'
#' \subsection{Multi-Domain Binding}{
#' When multiple source domains are specified, the function formats them into
#' a proper `rbind()` call with line breaks for readability in generated code.
#' Single domains are used directly without rbind wrapping.
#' }
#'
#' \subsection{SRC_ Variable Handling}{
#' If "SRC_" appears in `keep_vars`, the function prepares mutation specifications
#' for each source domain. This enables tracking which source domain contributed
#' each record in the final ADaM dataset.
#' }
#'
#' \subsection{Template Integration}{
#' The returned parameters are designed to integrate with code generation
#' templates that produce initialization blocks like:
#' \preformatted{
#' ADSL <- DM |>
#'   select(keep_vars) |>
#'   mutate(SRC_= "DM")
#' }
#' }
#'
#' @section Generated Code Pattern:
#' The parameters typically generate initialization code following this pattern:
#' \itemize{
#'   \item Combine source domains using rbind if multiple domains
#'   \item Select specified columns using `keep_vars`
#'   \item Add SRC_ tracking variables if requested
#'   \item Assign result to the target ADaM domain variable
#' }
#'
#' @examples
#' \dontrun{
#' # Single source domain initialization
#' params_single <-  params_init_domain_code(
#'   .self = "ADSL",
#'   keep_vars = c("USUBJID", "AGE", "SEX"),
#'   source_domains = "DM"
#' )
#'
#' # Multiple source domains with SRC_ tracking
#' params_multi <- params_init_domain_code(
#'   .self = "ADLB",
#'   keep_vars = c("USUBJID", "PARAMCD", "AVAL", "SRC_"),
#'   source_domains = c("LB", "VS")
#' )
#'
#' # Examine the formatted rbind expression
#' cat(params_multi$source_domain_rbind)
#' # rbind(LB,
#' #       VS)
#'
#' # Check SRC_ mutations
#' str(params_multi$src_mutations)
#' # List of 2
#' #  $ :List of 1
#' #   ..$ domain: chr "LB"
#' #  $ :List of 1
#' #   ..$ domain: chr "VS"
#' }
#'
#' @seealso
#' [define_params()] for the parent function that prepare the parameters for all
#' actions
#' @noRd
params_init_domain_code <- function(.self, keep_vars, source_domains) {
  # Initialize ADaM table by row binding source domain(s) and selecting
  # predecessors from source domain(s).

  # Data preparation for SRC_ mutations
  src_mutations <- list()
  if ("SRC_" %in% keep_vars) {
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

  return(list(
    self = .self,
    keep_vars = paste(keep_vars, collapse = ", "),
    source_domain_rbind = source_domain_rbind,
    src_mutations = src_mutations
  ))
}
