#' @title Copy rows 01
#' @description A description 
#' @type derivation
#' @returns `ADLB`
#' @code
copy_rows_01 <-   function(ADLB) {
  ADLB <- rbind(ADLB, ADLB[1:10,])
  return(ADLB)
}

