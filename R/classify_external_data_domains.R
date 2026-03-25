#' Classify Data Domain Names by Type
#'
#' @description
#' Classifies domain names into categories (SDTM, ADaM, metadata)
#' based on clinical data naming conventions.
#'
#' @param vector Character vector of domain names to classify.
#'
#' @return
#' Character vector with classifications:
#' \itemize{
#'   \item `"sdtm"` - 2-char domains, "relrec", "dm_*", "supp*"
#'   \item `"adam"` - Names starting with "ad"
#'   \item `"md"` - Names starting with "md"
#'   \item `NA` - Domains that don't match any recognized pattern
#' }
#'
#' @noRd
classify_data_domains <- function(vector) {
  invalid <- vector[!grepl("^[a-zA-Z][a-zA-Z0-9_]+$", vector)]
  if (length(invalid) > 0) {
    n_invalid <- length(invalid)
    invalid_list <- format_list(invalid, format_domain)
    invalid_msg <- paste0(
      cli::format_inline("{cli::qty(n_invalid)}Invalid domain name{?s}: "),
      invalid_list
    )

    throw_validation_error(
      category = "Invalid domain names",
      details = c(
        "Domain names must start with a letter and contain only letters, digits, or underscores",
        "x" = invalid_msg
      ),
      suggestions = c(
        "Ensure all domain names follow standard naming conventions",
        "Check for special characters or spaces in domain names",
        "Verify domain names start with a letter (A-Z or a-z)"
      )
    )
  }
  lc <- tolower(vector)
  data.table::fcase(
    nchar(lc) == 2 | grepl("^(relrec$|dm_|supp)", lc),
    "sdtm",
    grepl("^ad", lc),
    "adam",
    grepl("^md", lc),
    "md",
    default = NA_character_
  )
}
