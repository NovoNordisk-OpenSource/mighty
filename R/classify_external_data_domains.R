classify_data_domains <- function(vector) {
  result <- character(length(vector))
  result[!grepl("\\.", vector, ignore.case = TRUE)] <- "self"
  result[grepl("^ad", vector, ignore.case = TRUE)] <-  "adam"
  result[grepl("^md", vector, ignore.case = TRUE)] <- "md"
  result[nchar(vector) == 2 | vector == "relrec" | grepl("^dm_", vector, ignore.case = TRUE) | grepl("^supp", vector, ignore.case = TRUE)] <- "sdtm"
  result[vector == "core"] <- "core"

  # Check for unclassified values
  unclassified <- which(result == "")
  if(length(unclassified) > 0) {
    stop("classify_data_domains: Unknown domain(s): ",
         paste(vector[unclassified], collapse=", "))
  }
  return(result)
}
