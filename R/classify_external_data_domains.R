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
    cli::cli_abort(c(
      "Domain names must start with a letter and contain only letters, digits, or underscores",
      "x" = "Invalid domain name{?s}: {.val {invalid}}"
    ))
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
