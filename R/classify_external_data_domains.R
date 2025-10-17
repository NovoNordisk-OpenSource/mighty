#' Classify Data Domain Names by Type
#'
#' @description
#' Classifies domain names into categories (SDTM, ADaM, metadata, core, self)
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
#'   \item `"core"` - Exactly "core"
#'   \item `"self"` - Other names without dots
#' }
#'
#' @details
#' Throws error for unrecognized domain names.
classify_data_domains <- function(vector) {
  result <- character(length(vector))
  result[!grepl("\\.", vector, ignore.case = TRUE)] <- "self"
  result[grepl("^ad", vector, ignore.case = TRUE)] <- "adam"
  result[grepl("^md", vector, ignore.case = TRUE)] <- "md"
  result[
    nchar(vector) == 2 |
      vector == "relrec" |
      grepl("^dm_", vector, ignore.case = TRUE) |
      grepl("^supp", vector, ignore.case = TRUE)
  ] <- "sdtm"
  result[vector == "core"] <- "core"

  # Check for unclassified values
  unclassified <- which(result == "")
  if (length(unclassified) > 0) {
    stop(
      "classify_data_domains: Unknown domain(s): ",
      paste(vector[unclassified], collapse = ", ")
    )
  }
  return(result)
}
