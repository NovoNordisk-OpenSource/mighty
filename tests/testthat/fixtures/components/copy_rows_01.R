#' @title Copy rows 01
#' @description A description 
#' @type col_compute
#' @returns `ADLB`
copy_rows_01 <-   function(ADLB) {
  ADLB <- rbind(ADLB, ADLB[1:10,])
  return(ADLB)
}

