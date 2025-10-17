#' Generate Parameters for ADaM Domain Initialization Code
#'
#' This function prepares template parameters for generating R code that initializes
#' ADaM domains. It handles the complexities of multi-domain binding, column
#' selection, and SRC_ variable creation for use in code generation templates.
#'
#' @param .self Character. The target ADaM domain name (e.g., "adsl", "adae").
#'   Used as the variable name for the initialized data set and converted to
#'   uppercase for data set labels and references.
#' @param keep_columns Character vector. Column names to retain in the initialized
#'   ADaM domain. If "SRC_" is included, source domain tracking variables will
#'   be generated for each contributing SDTM domain.
#' @param source_domains Character vector. Names of source SDTM domains to combine
#'   (e.g., c("dm", "vs", "lb")). These domains will be row-bound together to
#'   create the initial ADaM data set structure.
#'
#' @return A named list containing template parameters for code generation:
#'   \describe{
#'     \item{self}{Character. The target ADaM domain name (same as `.self`)}
#'     \item{self_upper}{Character. Uppercase version of the domain name for labels}
#'     \item{keep_columns}{Character vector. Columns to retain (same as input)}
#'     \item{source_domain_rbind}{Character. Formatted R expression for combining
#'       source domains. Single domain returns the domain name; multiple domains
#'       return a formatted `rbind()` call}
#'     \item{src_mutations}{List. Source domain mutation specifications for SRC_
#'       variable creation. Empty list if "SRC_" not in `keep_columns`}
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
#' If "SRC_" appears in `keep_columns`, the function prepares mutation specifications
#' for each source domain. This enables tracking which source domain contributed
#' each record in the final ADaM dataset.
#' }
#'
#' \subsection{Template Integration}{
#' The returned parameters are designed to integrate with code generation
#' templates that produce initialization blocks like:
#' \preformatted{
#' adsl <-  dm |>
#'   select(keep_columns) |>
#'   mutate(SRC_= "DM")
#' }
#' }
#'
#' @section Generated Code Pattern:
#' The parameters typically generate initialization code following this pattern:
#' \itemize{
#'   \item Combine source domains using rbind if multiple domains
#'   \item Select specified columns using `keep_columns`
#'   \item Add SRC_ tracking variables if requested
#'   \item Assign result to the target ADaM domain variable
#' }
#'
#' @examples
#' \dontrun{
#' # Single source domain initialization
#' params_single <-  params_init_domain_code(
#'   .self = "adsl",
#'   keep_columns = c("USUBJID", "AGE", "SEX"),
#'   source_domains = "dm"
#' )
#'
#' # Multiple source domains with SRC_ tracking
#' params_multi <- params_init_domain_code(
#'   .self = "adlb",
#'   keep_columns = c("USUBJID", "PARAMCD", "AVAL", "SRC_"),
#'   source_domains = c("lb", "vs")
#' )
#'
#' # Examine the formatted rbind expression
#' cat(params_multi$source_domain_rbind)
#' # rbind(lb,
#' #       vs)
#'
#' # Check SRC_ mutations
#' str(params_multi$src_mutations)
#' # List of 2
#' #  $ :List of 1
#' #   ..$ domain: chr "lb"
#' #  $ :List of 1
#' #   ..$ domain: chr "vs"
#' }
#'
#' @seealso
#' [define_params()] for the parent function that prepare the parameters for all
#' actions
#'
params_init_domain_code <- function(.self, keep_columns, source_domains) {
  # Initialize ADaM table by row binding source domain(s) and selecting
  # predecessors from source domain(s).

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

  return(list(
    self = .self,
    self_upper = toupper(.self),
    keep_columns = keep_columns,
    source_domain_rbind = source_domain_rbind,
    src_mutations = src_mutations
  ))
}
