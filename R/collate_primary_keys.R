#' Collate primary keys
#'
#' @description
#' Collects the primary key information produces a single list
#'
#' @returns a list where each element is a named vector conaing the priarmy keys
#'   for that domain
collate_primary_keys <- function(trial_metadata_keys){
  c(trial_metadata_keys$primary_keys_sdtm, trial_metadata_keys$primary_keys_adam, trial_metadata_keys$primary_keys_md)
}
