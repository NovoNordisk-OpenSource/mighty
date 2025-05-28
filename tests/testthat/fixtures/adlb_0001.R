#' Lbtest 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self USUBJID
#' @depends .self LBSEQ
#' @depends .self AVAL
#' @depends LB USUBJID
#' @depends LB LBSEQ
#' @depends LB LBTEST
#' @depends ADSL USUBJID
#' @depends ADSL PLANNED_ARM
#' @outputs LBTEST
#' @returns `.self`
lbtest_01 <-   function(.self, LB, ADSL) {

  # join with adsl, only derive lbtest if adsl.planned_arm != "" and
  # if aval is na them lbtest = "Invalid"
  .self <- .self |>
    dplyr::left_join(
      LB |> dplyr::select(USUBJID, LBSEQ, LBTEST),
      by = c("USUBJID" = "USUBJID", "LBSEQ" = "LBSEQ")
    ) |>
    dplyr::left_join(
      ADSL |> dplyr::select(USUBJID, PLANNED_ARM),
      by = "USUBJID"
    ) |>
    dplyr::mutate(
      LBTEST = ifelse(
        !is.na(AVAL) & PLANNED_ARM != "",
        LBTEST,
        "Invalid"
      )
    ) |>
    dplyr::select(-PLANNED_ARM)

  return(.self)
}


#' New lbtest 01
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @depends SV VISITNUM
#' @depends SV USUBJID
#' @depends SV VISITDY
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_01 <-   function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Microcytes") |>
    dplyr::mutate(LBTEST = "Microcytes (new)")

  new_lbtest <-   new_lbtest |>
    dplyr::left_join(SV, by = c("USUBJID", "VISITNUM")) |>
    dplyr::filter(VISITDY > 20) |>
    dplyr::select(-VISITDY)

  .self <-   rbind(.self, new_lbtest)
  return(.self)
}

#' New lbtest 02
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @depends .self AVALC
#' @outputs LBTEST
#' @outputs AVALC
#' @returns `.self`
new_lbtest_02 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Macrocytes" &
                    as.numeric(gsub("[N<]",   "", AVALC))>0.5 &
                    !is.na(AVALC)) |>
    dplyr::mutate(LBTEST = "Macrocytes (new)",
                  AVALC = "")

  .self <-   rbind(.self, new_lbtest)
  return(.self)
}

#' New lbtest 03
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_03 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_lbtest)
  return(.self)
}

#' New lbtest 04
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_04 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_lbtest)
  return(.self)
}

#' New lbtest 05
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_05 <- function(.self) {

  new_lbtest1 <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  new_lbtest2 <-   .self |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_lbtest1, new_lbtest2)
  return(.self)
}

#' New lbtest 06
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @depends .self AVAL
#' @depends .self DOMAIN
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_06 <- function(.self) {

  new_lbtest1 <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)" & AVAL>1 & !is.na(DOMAIN)) |>
    dplyr::mutate(LBTEST = "Microcytes (new 3)")

  new_lbtest2 <-   .self |>
    dplyr::filter(LBTEST == "Microcytes (new 2)" & AVAL>1 & !is.na(DOMAIN)) |>
    dplyr::mutate(LBTEST = "Microcytes (new 4)")


  if(nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_lbtest1, new_lbtest2)
  return(.self)
}

#' New aval 01
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self AVAL
#' @outputs AVAL
#' @returns `.self`
new_aval_01 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 1) |>
    dplyr::mutate(AVAL = 3.14)

  .self <-   rbind(.self, new_aval)
  return(.self)
}

#' New aval 02
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self AVAL
#' @outputs AVAL
#' @returns `.self`
new_aval_02 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 3.14) |>
    dplyr::mutate(AVAL = 0)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_aval)
  return(.self)
}

#' New aval 03
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self AVAL
#' @depends .self DOMAIN
#' @outputs AVAL
#' @returns `.self`
new_aval_03 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 2 & DOMAIN == "LB") |>
    dplyr::mutate(AVAL = 0)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_aval)
  return(.self)
}

#' New aval 04
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self AVAL
#' @depends .self AVALC
#' @outputs AVAL
#' @returns `.self`
new_aval_04 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL > 1000 & !is.na(AVAL) & AVALC != "") |>
    dplyr::mutate(AVAL = 1000)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_aval)
  return(.self)
}

#' New visitnum 01
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self VISITNUM
#' @outputs VISITNUM
#' @returns `.self`
new_visitnum_01 <- function(.self) {

  new_visitnum <- .self |>
    dplyr::filter(round(VISITNUM,2) == 1.3) |>
    dplyr::mutate(VISITNUM = 1.4)

  if(nrow(new_visitnum) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_visitnum)
  return(.self)
}

#' New visitnum 02
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self VISITNUM
#' @outputs VISITNUM
#' @returns `.self`
new_visitnum_02 <- function(.self) {

  new_visitnum <- .self |>
    dplyr::filter(VISITNUM == 1.4) |>
    dplyr::mutate(VISITNUM = 1.5)

  if(nrow(new_visitnum) == 0) {
    stop("No rows to add.")
  }

  .self <-   rbind(.self, new_visitnum)
  return(.self)
}

#' Aval grp 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AVAL
#' @outputs AVAL_GRP
#' @returns `.self`
aval_grp_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AVAL_GRP = ifelse(
      AVAL < 10, "low",
      ifelse(AVAL < 100, "medium", "high")
    ))
  return(.self)
}

#' Aval grp 02
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @depends .self AVAL_GRP
#' @outputs AVAL_GRP2
#' @returns `.self`
aval_grp_02 <-   function(.self) {
  .self <- .self |>
    dplyr::mutate(AVAL_GRP2 = ifelse(
      AVAL_GRP %in% c("low", "medium"), "low/medium", "high")
    )
  return(.self)
}


#' Copy rows 01
#' @param .self `data.frame` Input data set
#' @type col_compute
#' @returns `.self`
copy_rows_01 <-   function(.self) {
  .self <- rbind(.self, .self[1:10,])
  return(.self)
}

#' New lbtest 07
#' @param .self `data.frame` Input data set
#' @type row_compute
#' @depends .self LBTEST
#' @outputs LBTEST
#' @returns `.self`
new_lbtest_07 <- function(.self, params = list(test_val = "Phosphate")) {
  if (!is.null(params$test_val)) {
    new_lbtest <- .self |>
      dplyr::filter(LBTEST == params$test_val) |>
      dplyr::mutate(LBTEST = paste0(params$test_val, "_new"))
  } else {
    new_lbtest <- data.table()
  }

  .self <- rbind(.self, new_lbtest)
  return(.self)
}
