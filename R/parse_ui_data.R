#' Convienence function to parse the data from the UI
#' @description Performs various data integrity checks and actions. Ensurse NAs
#' are passed as R NA_character_ and not as strings of "na".
#'
#' @return
#' @export
#'
#' @examples
parse_ui_data <- function(yml_data){
  # Ensure NAs are passed as R NA_character_ and not as strings of "na"

  convert_to_NA_character(yml_data)


  # Ensure that the data is
}


convert_to_NA_character <- function(x) {
  # Check if x is a list
  if (is.list(x)) {
    # Apply recursively
    return(lapply(x, convert_to_NA_character))
  }


  if (is.character(x) && any(x == "NA")) {
    x[x == "NA"] <- NA_character_
  }

  # Return the possibly modified x
  return(x)
}
