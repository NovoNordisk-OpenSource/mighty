# Read data sets ------------------------------------------------

cnt <- connector::connect(
  config = '/var/folders/gg/6xrmz_gd51lgdqyz00p2kcbr0000gp/T//RtmptFplc8/filea7d99111c07/_connector.yml'
)
lb <- cnt$sdtm$read_cnt('lb') |>
  dplyr::select(
    DOMAIN,
    LBSEQ,
    LBSTRESC,
    LBSTRESN,
    LBTEST,
    STUDYID,
    USUBJID,
    VISITNUM
  )
ADSL <- cnt$adam$read_cnt('ADSL') |>
  dplyr::select(PLANNED_ARM, RACE_COUNTRY, SEX, STUDYID, USUBJID)
sv <- cnt$sdtm$read_cnt('sv') |>
  dplyr::select(USUBJID, VISITDY, VISITNUM)


# Initialize ADLB ----------------------

ADLB <- lb |>
  admiral::convert_blanks_to_na()


# Filter ADLB ----------------------

ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |> dplyr::select(SEX, USUBJID, STUDYID),
    by = c("USUBJID", "STUDYID")
  )

ADLB <- ADLB |>
  dplyr::filter(!is.na(SEX))

ADLB <- ADLB |>
  dplyr::select(USUBJID, VISITNUM, STUDYID, LBSEQ, LBSTRESN, LBSTRESC, DOMAIN)


# ADLB-AVAL-_COL_MUTATE -----------------------------------------------------

ADLB <- ADLB |> dplyr::mutate(AVAL = LBSTRESN)


# ADLB-AVAL_GRP-./fixtures/components/aval_grp_01.R
ADLB <- dplyr::mutate(
  ADLB,
  AVAL_GRP = ifelse(AVAL < 10, "low", ifelse(AVAL < 100, "medium", "high"))
)

# ADLB-AVAL_GRP2-./fixtures/components/aval_grp_02.R
ADLB <- dplyr::mutate(
  ADLB,
  AVAL_GRP2 = ifelse(
    AVAL_GRP %in%
      c("low", "medium"),
    "low/medium",
    "high"
  )
)

# ADLB-LBTEST-./fixtures/components/lbtest_01.R
ADLB <- dplyr::select(
  dplyr::mutate(
    dplyr::left_join(
      dplyr::left_join(
        ADLB,
        dplyr::select(lb, USUBJID, LBSEQ, LBTEST),
        by = c(
          USUBJID = "USUBJID",
          LBSEQ = "LBSEQ"
        )
      ),
      dplyr::select(ADSL, USUBJID, PLANNED_ARM),
      by = "USUBJID"
    ),
    LBTEST = ifelse(!is.na(AVAL) & PLANNED_ARM != "", LBTEST, "Invalid")
  ),
  -PLANNED_ARM
)

# ADLB-LBTEST-./fixtures/components/new_lbtest_01.R
new_lbtest <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Microcytes"),
  LBTEST = "Microcytes (new)"
)
new_lbtest <- dplyr::select(
  dplyr::filter(
    dplyr::left_join(new_lbtest, sv, by = c("USUBJID", "VISITNUM")),
    VISITDY > 20
  ),
  -VISITDY
)
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-LBTEST-./fixtures/components/new_lbtest_03.R
new_lbtest <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Microcytes (new)"),
  LBTEST = "Microcytes (new 2)"
)
if (nrow(new_lbtest) == 0) {}
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-LBTEST-./fixtures/components/new_lbtest_04.R
new_lbtest <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Macrocytes (new)"),
  LBTEST = "Macrocytes (new 2)"
)
if (nrow(new_lbtest) == 0) {}
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-LBTEST-./fixtures/components/new_lbtest_05.R
new_lbtest1 <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Microcytes (new)"),
  LBTEST = "Microcytes (new 2)"
)
new_lbtest2 <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Macrocytes (new)"),
  LBTEST = "Macrocytes (new 2)"
)
if (nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {}
ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)

# ADLB-LBTEST-./fixtures/components/new_lbtest_06.R
new_lbtest1 <- dplyr::mutate(
  dplyr::filter(ADLB, LBTEST == "Microcytes (new)" & AVAL > 1 & !is.na(DOMAIN)),
  LBTEST = "Microcytes (new 3)"
)
new_lbtest2 <- dplyr::mutate(
  dplyr::filter(
    ADLB,
    LBTEST == "Microcytes (new 2)" & AVAL > 1 & !is.na(DOMAIN)
  ),
  LBTEST = "Microcytes (new 4)"
)
if (nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {}
ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)

# ADLB-LBTEST-./fixtures/components/new_lbtest_07.R
if (!is.null("Protein")) {
  new_lbtest <- dplyr::mutate(
    dplyr::filter(ADLB, LBTEST == "Protein"),
    LBTEST = paste0("Protein", "_new")
  )
} else {
  new_lbtest <- data.table()
}
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-LBTEST-./fixtures/components/new_lbtest_08.R
if (!is.null("Specific Gravity")) {
  new_lbtest <- dplyr::mutate(
    dplyr::filter(ADLB, LBTEST == "Specific Gravity"),
    LBTEST = paste0(
      "Specific Gravity",
      "_new"
    )
  )
} else {
  new_lbtest <- data.table()
}
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-AVAL-./fixtures/components/new_aval_01.R
new_aval <- dplyr::mutate(dplyr::filter(ADLB, AVAL == 1), AVAL = 3.14)
ADLB <- rbind(ADLB, new_aval)

# ADLB-AVAL-./fixtures/components/new_aval_02.R
new_aval <- dplyr::mutate(dplyr::filter(ADLB, AVAL == 3.14), AVAL = 0)
if (nrow(new_aval) == 0) {}
ADLB <- rbind(ADLB, new_aval)

# ADLB-AVAL-./fixtures/components/new_aval_03.R
new_aval <- dplyr::mutate(
  dplyr::filter(
    ADLB,
    AVAL == 2 &
      DOMAIN == "LB"
  ),
  AVAL = 0
)
if (nrow(new_aval) == 0) {}
ADLB <- rbind(ADLB, new_aval)

# ADLB-AVALC-_COL_MUTATE -----------------------------------------------------

ADLB <- ADLB |> dplyr::mutate(AVALC = LBSTRESC)


# ADLB-AVAL-./fixtures/components/new_aval_04.R
new_aval <- dplyr::mutate(
  dplyr::filter(
    ADLB,
    AVAL > 1000 &
      !is.na(AVAL) &
      AVALC != ""
  ),
  AVAL = 1000
)
if (nrow(new_aval) == 0) {}
ADLB <- rbind(ADLB, new_aval)

# ADLB-LBTEST-AVALC-./fixtures/components/new_lbtest_02.R
new_lbtest <- dplyr::mutate(
  dplyr::filter(
    ADLB,
    LBTEST == "Macrocytes" &
      as.numeric(gsub("[N<]", "", AVALC)) > 0.5 &
      !is.na(AVALC)
  ),
  LBTEST = "Macrocytes (new)",
  AVALC = ""
)
ADLB <- rbind(ADLB, new_lbtest)

# ADLB-VISITNUM-./fixtures/components/new_visitnum_01.R
new_visitnum <- dplyr::mutate(
  dplyr::filter(
    ADLB,
    round(
      VISITNUM,
      2
    ) ==
      1.3
  ),
  VISITNUM = 1.4
)
if (nrow(new_visitnum) == 0) {}
ADLB <- rbind(ADLB, new_visitnum)

# ADLB-VISITNUM-./fixtures/components/new_visitnum_02.R
new_visitnum <- dplyr::mutate(
  dplyr::filter(ADLB, VISITNUM == 1.4),
  VISITNUM = 1.5
)
if (nrow(new_visitnum) == 0) {}
ADLB <- rbind(ADLB, new_visitnum)
# ADLB-ARM-_col_echo -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |> dplyr::select(USUBJID, STUDYID, PLANNED_ARM),
    by = c("USUBJID", "STUDYID")
  ) |>
  dplyr::rename(ARM = PLANNED_ARM)

# ADLB-RACE_COUNTRY-_col_echo -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |> dplyr::select(USUBJID, STUDYID, RACE_COUNTRY),
    by = c("USUBJID", "STUDYID")
  )


# Write to disk {self_upper} ------------------------------------------------
cnt$adam$write_cnt(ADLB, "ADLB.parquet", overwrite = TRUE)
