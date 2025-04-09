#' Classify external data domains
#'
#' @param vector
#'
#' @return
#' @export
#'
#' @examples
classify_external_data_domains_2 <- function(vector) {
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
    stop("classify_external_data_domains: Unknown domain \"",
         x,
         "\"")
  }, FUN.VALUE = character(1)) |> unname()

  return(classified_vector)
}
