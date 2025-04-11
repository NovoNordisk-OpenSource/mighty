#!-!
# type: derivation
# depend_cols:
#   - self.USUBJID
#   - self.LBSEQ
#   - self.AVAL
#   - lb.USUBJID
#   - lb.LBSEQ
#   - lb.LBTEST
#   - adsl.USUBJID
#   - adsl.PLANNED_ARM
# outputs:
#   - LBTEST
#!-!
lbtest_01 <- function(.self, lb, adsl) {

  # join with adsl, only derive lbtest if adsl.planned_arm != "" and
  # if aval is na them lbtest = "Invalid"
  .self <- .self |>
    dplyr::left_join(
      lb |> dplyr::select(USUBJID, LBSEQ, LBTEST),
      by = c("USUBJID" = "USUBJID", "LBSEQ" = "LBSEQ")
    ) |>
    dplyr::left_join(
      adsl |> dplyr::select(USUBJID, PLANNED_ARM),
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


#!-!
# type: row
# depend_cols:
#   - self.LBTEST
# outputs:
#   - LBTEST
#!-!
new_lbtest_01 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Microcytes") |>
    dplyr::mutate(LBTEST = "Microcytes (new)")

  .self <- rbind(.self, new_lbtest)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.LBTEST
#   - self.AVALC
# outputs:
#   - LBTEST
#   - AVALC
#!-!
new_lbtest_02 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Macrocytes" &
                    as.numeric(gsub("[N<]", "", AVALC))>0.5 &
                    !is.na(AVALC)) |>
    dplyr::mutate(LBTEST = "Macrocytes (new)",
                  AVALC = "")

  .self <- rbind(.self, new_lbtest)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.LBTEST
# outputs:
#   - LBTEST
#!-!
new_lbtest_03 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_lbtest)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.LBTEST
# outputs:
#   - LBTEST
#!-!
new_lbtest_04 <- function(.self) {

  new_lbtest <- .self |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_lbtest)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.LBTEST
# outputs:
#   - LBTEST
#!-!
new_lbtest_05 <- function(.self) {

  new_lbtest1 <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)") |>
    dplyr::mutate(LBTEST = "Microcytes (new 2)")

  new_lbtest2 <- .self |>
    dplyr::filter(LBTEST == "Macrocytes (new)") |>
    dplyr::mutate(LBTEST = "Macrocytes (new 2)")

  if(nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_lbtest1, new_lbtest2)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.LBTEST
#   - self.AVAL
# outputs:
#   - LBTEST
#!-!
new_lbtest_06 <- function(.self) {

  new_lbtest1 <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new)" & AVAL>1) |>
    dplyr::mutate(LBTEST = "Microcytes (new 3)")

  new_lbtest2 <- .self |>
    dplyr::filter(LBTEST == "Microcytes (new 2)" & AVAL>1) |>
    dplyr::mutate(LBTEST = "Microcytes (new 4)")


  if(nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_lbtest1, new_lbtest2)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.AVAL
# outputs:
#   - AVAL
#!-!
new_aval_01 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 1) |>
    dplyr::mutate(AVAL = 3.14)

  .self <- rbind(.self, new_aval)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.AVAL
# outputs:
#   - AVAL
#!-!
new_aval_02 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 3.14) |>
    dplyr::mutate(AVAL = 0)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_aval)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.AVAL
#   - self.DOMAIN
# outputs:
#   - AVAL
#!-!
new_aval_03 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL == 2 & DOMAIN == "LB") |>
    dplyr::mutate(AVAL = 0)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_aval)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.AVAL
#   - self.AVALC
# outputs:
#   - AVAL
#!-!
new_aval_04 <- function(.self) {

  new_aval <- .self |>
    dplyr::filter(AVAL > 1000 & !is.na(AVAL) & AVALC != "") |>
    dplyr::mutate(AVAL = 1000)

  if(nrow(new_aval) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_aval)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.VISITNUM
# outputs:
#   - VISITNUM
#!-!
new_visitnum_01 <- function(.self) {

  new_visitnum <- .self |>
    dplyr::filter(round(VISITNUM,2) == 1.3) |>
    dplyr::mutate(VISITNUM = 1.4)

  if(nrow(new_visitnum) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_visitnum)
  return(.self)
}

#!-!
# type: row
# depend_cols:
#   - self.VISITNUM
# outputs:
#   - VISITNUM
#!-!
new_visitnum_02 <- function(.self) {

  new_visitnum <- .self |>
    dplyr::filter(VISITNUM == 1.4) |>
    dplyr::mutate(VISITNUM = 1.5)

  if(nrow(new_visitnum) == 0) {
    stop("No rows to add.")
  }

  .self <- rbind(.self, new_visitnum)
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.AVAL
# outputs:
#   - AVAL_GRP
#!-!
aval_grp_01 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AVAL_GRP = ifelse(
      AVAL < 10, "low",
      ifelse(AVAL < 100, "medium", "high")
    ))
  return(.self)
}

#!-!
# type: derivation
# depend_cols:
#   - self.AVAL_GRP
# outputs:
#   - AVAL_GRP2
#!-!
aval_grp_02 <- function(.self) {
  .self <- .self |>
    dplyr::mutate(AVAL_GRP2 = ifelse(
      AVAL_GRP %in% c("low", "medium"), "low/medium", "high")
    )
  return(.self)
}


#!-!
# type: derivation
# depend_cols:
# outputs:
#!-!
copy_rows_01 <- function(.self) {
  .self <- rbind(.self, .self[1:10,])
  return(.self)
}
