#' Update the depend_rows in a data.table by prefixing with the domain.
#'
#' This function takes a data.table containing a column `depend_rows` and
#' a column `domain`. It updates the `depend_rows` by prepending the
#' corresponding `domain` value to each element in `depend_rows` for rows
#' where `depend_rows` is not NA.
#'
#' @param nodes A data.table with at least two columns:
#'   \describe{
#'     \item{depend_rows}{A list or character vector containing dependent rows.}
#'     \item{domain}{A character vector used as a prefix.}
#'   }
#'
#' @return A data.table with updated `depend_rows` where prefixes are added.
#'
update_depend_rows <- function(nodes) {
  #  Make a copy of the input data.table to preserve original data
  x <- copy(nodes)

  # Update depend_rows by prepending the corresponding domain prefix
  x[
    !is.na(depend_rows),
    depend_rows := lapply(seq_len(.N), function(i) {
      paste0(domain[i], "-", depend_rows[[i]]) # Concatenate domain and depend_rows
    })
  ]

  # Return the updated data.table
  return(x)
}
