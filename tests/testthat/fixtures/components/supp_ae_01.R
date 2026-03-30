#' @title supp_ae_01
#' @description A description
#' @type derivation
#' @depends ADAE USUBJID
#' @depends ADAE AESEQ
#' @depends SUPPAE USUBJID
#' @depends SUPPAE IDVAR
#' @depends SUPPAE IDVARVAL
#' @depends SUPPAE QNAM
#' @depends SUPPAE QVAL
#' @outputs AETRTEM
#' @code
# Collect supplementary data
supp_data <- SUPPAE |>
  dplyr::select(USUBJID, IDVAR, IDVARVAL, QNAM, QVAL) |>
  dplyr::filter(QNAM == "AETRTEM") |>
  tidyr::pivot_wider(names_from = QNAM, values_from = QVAL)

idvar <- unique(supp_data$IDVAR)
idclass <- class(ADAE[[idvar]])

# Join supplementary data
supp_data[["IDVARVAL"]] <- do.call(
  what = get(paste0("as.", idclass)),
  args = list(supp_data$IDVARVAL)
)
ADAE <- ADAE |>
  dplyr::left_join(
    supp_data |> dplyr::select(-IDVAR),
    by = c("USUBJID", stats::setNames("IDVARVAL", idvar))
  )
