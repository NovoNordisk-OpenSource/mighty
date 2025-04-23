#' Classify external data domains
#'
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
classify_external_data_domains_2 <- function(vector) {
  browser()
  classified_vector <- vapply(vector, function(x) {
    if (grepl("^ad", x)) {
      return("adam")
    }
    if (grepl("^md", x)) {
      return("md")
    }
    if(is.null(x)|is.na(x))browser()
    if (nchar(x) == 2 || x=="relrec" || substr(x,1,3) =="dm_") {
      return("sdtm")
    }
    if (x == "self") {
      return("self")
    }
    browser()
    stop("classify_external_data_domains: Unknown domain \"",
         x,
         "\"")
  }, FUN.VALUE = character(1)) |> unname()

  return(classified_vector)
}

classify_external_data_domains <- function(vector) {
  result <- character(length(vector))
  result[!grepl("\\.", vector, ignore.case = TRUE)] <- "self"
  result[grepl("^ad", vector, ignore.case = TRUE)] <-  "adam"
  result[grepl("^md", vector, ignore.case = TRUE)] <- "md"
  result[nchar(vector) == 2 | vector == "relrec" | grepl("^dm_", vector, ignore.case = TRUE)] <- "sdtm"
  result[vector == "core"] <- "core"

  # Check for unclassified values
  unclassified <- which(result == "")
  if(length(unclassified) > 0) {
    stop("classify_external_data_domains: Unknown domain(s): ",
         paste(vector[unclassified], collapse=", "))
  }
  return(result)
}
