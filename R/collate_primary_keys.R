#' Collate Primary Keys
#'
#' @description
#' Combines primary key information from SDTM, ADaM, and metadata sources
#' into a single consolidated list.
#'
#' @param trial_metadata_keys List containing primary key information with
#'   elements `primary_keys_sdtm`, `primary_keys_adam`, and `primary_keys_md`.
#'
#' @return
#' A list where each element is a named vector containing the primary keys
#' for that domain, combining all three data sources.
#'
#' @noRd
collate_primary_keys <- function(trial_metadata_keys) {
  c(
    trial_metadata_keys$primary_keys_sdtm,
    trial_metadata_keys$primary_keys_adam,
    trial_metadata_keys$primary_keys_md
  )
}
