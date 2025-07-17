# Read data sets ------------------------------------------------

cnt <- connector::connect(
  config = "/var/folders/gg/6xrmz_gd51lgdqyz00p2kcbr0000gp/T//RtmpOmoQyb/filec3ee3fffe95f/_connector.yml"
)

ADSL <- cnt$adam$read_cnt("ADSL") |>
  dplyr::select(PLANNED_ARM, RACE_COUNTRY, SEX, STUDYID, USUBJID)

lb <- cnt$sdtm$read_cnt("lb") |>
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

sv <- cnt$sdtm$read_cnt("sv") |> dplyr::select(USUBJID, VISITDY, VISITNUM)

# Initialize ADLB ----------------------

ADLB <- lb |>
  admiral::convert_blanks_to_na()

# Preprocess ADLB ------------------------------

# Add ADSL columns for filtering
ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |>
      dplyr::select(SEX, USUBJID, STUDYID),
    by = c("USUBJID", "STUDYID")
  )

# Apply global filter
ADLB <- ADLB |>
  dplyr::filter(!is.na(SEX))

# Select ADLB predecessors
ADLB <- ADLB |>
  dplyr::select(USUBJID, VISITNUM, LBSTRESN, LBSTRESC, STUDYID, LBSEQ, DOMAIN)

# ADLB-ARM -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |> dplyr::select(USUBJID, STUDYID, PLANNED_ARM),
    by = c("USUBJID", "STUDYID")
  ) |>
  dplyr::rename(ARM = PLANNED_ARM)

# ADLB-RACE_COUNTRY -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::left_join(
    ADSL |> dplyr::select(USUBJID, STUDYID, RACE_COUNTRY),
    by = c("USUBJID", "STUDYID")
  )

# ADLB-AVAL -----------------------------------------------------

ADLB <- ADLB |> dplyr::mutate(AVAL = LBSTRESN)

# ADLB-AVAL-new_aval_01 -----------------------------------------------------

new_aval <- ADLB |>
  dplyr::filter(AVAL == 1) |>
  dplyr::mutate(AVAL = 3.14)

ADLB <- rbind(ADLB, new_aval)

# Remove interim objects
rm(new_aval)

# ADLB-LBTEST-lbtest_01 -----------------------------------------------------

# join with adsl, only derive lbtest if adsl.planned_arm != "" and
# if aval is na them lbtest = "Invalid"
ADLB <- ADLB |>
  dplyr::left_join(
    lb |> dplyr::select(USUBJID, LBSEQ, LBTEST),
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


# ADLB-LBTEST-new_lbtest_01 -----------------------------------------------------

new_lbtest <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes") |>
  dplyr::mutate(LBTEST = "Microcytes (new)")

new_lbtest <- new_lbtest |>
  dplyr::left_join(sv, by = c("USUBJID", "VISITNUM")) |>
  dplyr::filter(VISITDY > 20) |>
  dplyr::select(-VISITDY)

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest)

# ADLB-AVAL_GRP-aval_grp_01 -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::mutate(
    AVAL_GRP = ifelse(
      AVAL < 10,
      "low",
      ifelse(AVAL < 100, "medium", "high")
    )
  )

# ADLB-AVAL_GRP2-aval_grp_02 -----------------------------------------------------

ADLB <- ADLB |>
  dplyr::mutate(
    AVAL_GRP2 = ifelse(
      AVAL_GRP %in% c("low", "medium"),
      "low/medium",
      "high"
    )
  )

# ADLB-LBTEST-new_lbtest_03 -----------------------------------------------------

new_lbtest <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes (new)") |>
  dplyr::mutate(LBTEST = "Microcytes (new 2)")

if (nrow(new_lbtest) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest)

# ADLB-LBTEST-new_lbtest_06 -----------------------------------------------------

new_lbtest1 <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes (new)" & AVAL > 1 & !is.na(DOMAIN)) |>
  dplyr::mutate(LBTEST = "Microcytes (new 3)")

new_lbtest2 <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes (new 2)" & AVAL > 1 & !is.na(DOMAIN)) |>
  dplyr::mutate(LBTEST = "Microcytes (new 4)")


if (nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)

# Remove interim objects
rm(new_lbtest1, new_lbtest2)

# ADLB-LBTEST-new_lbtest_07 -----------------------------------------------------
params <- list(test_val = "Phosphate")

if (!is.null(params$test_val)) {
  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == params$test_val) |>
    dplyr::mutate(LBTEST = paste0(params$test_val, "_new"))
} else {
  new_lbtest <- data.table()
}

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest, params)

# ADLB-LBTEST-new_lbtest_07-list(test_val = "s:Protein") -----------------------------------------------------
params <- list(test_val = "Protein")

if (!is.null(params$test_val)) {
  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == params$test_val) |>
    dplyr::mutate(LBTEST = paste0(params$test_val, "_new"))
} else {
  new_lbtest <- data.table()
}

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest, params)

# ADLB-LBTEST-new_lbtest_07-list(test_val = "s:Specific Gravity") -----------------------------------------------------
params <- list(test_val = "Specific Gravity")

if (!is.null(params$test_val)) {
  new_lbtest <- ADLB |>
    dplyr::filter(LBTEST == params$test_val) |>
    dplyr::mutate(LBTEST = paste0(params$test_val, "_new"))
} else {
  new_lbtest <- data.table()
}

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest, params)

# ADLB-AVAL-new_aval_02 -----------------------------------------------------

new_aval <- ADLB |>
  dplyr::filter(AVAL == 3.14) |>
  dplyr::mutate(AVAL = 0)

if (nrow(new_aval) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_aval)

# Remove interim objects
rm(new_aval)

# ADLB-AVAL-new_aval_03 -----------------------------------------------------

new_aval <- ADLB |>
  dplyr::filter(AVAL == 2 & DOMAIN == "LB") |>
  dplyr::mutate(AVAL = 0)

if (nrow(new_aval) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_aval)

# Remove interim objects
rm(new_aval)

# ADLB-AVALC -----------------------------------------------------

ADLB <- ADLB |> dplyr::mutate(AVALC = LBSTRESC)

# ADLB-AVAL-new_aval_04 -----------------------------------------------------

new_aval <- ADLB |>
  dplyr::filter(AVAL > 1000 & !is.na(AVAL) & AVALC != "") |>
  dplyr::mutate(AVAL = 1000)

if (nrow(new_aval) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_aval)

# Remove interim objects
rm(new_aval)

# ADLB-LBTEST-AVALC-new_lbtest_02 -----------------------------------------------------

new_lbtest <- ADLB |>
  dplyr::filter(
    LBTEST == "Macrocytes" &
      as.numeric(gsub("[N<]", "", AVALC)) > 0.5 &
      !is.na(AVALC)
  ) |>
  dplyr::mutate(
    LBTEST = "Macrocytes (new)",
    AVALC = ""
  )

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest)

# ADLB-LBTEST-new_lbtest_04 -----------------------------------------------------

new_lbtest <- ADLB |>
  dplyr::filter(LBTEST == "Macrocytes (new)") |>
  dplyr::mutate(LBTEST = "Macrocytes (new 2)")

if (nrow(new_lbtest) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_lbtest)

# Remove interim objects
rm(new_lbtest)

# ADLB-LBTEST-new_lbtest_05 -----------------------------------------------------

new_lbtest1 <- ADLB |>
  dplyr::filter(LBTEST == "Microcytes (new)") |>
  dplyr::mutate(LBTEST = "Microcytes (new 2)")

new_lbtest2 <- ADLB |>
  dplyr::filter(LBTEST == "Macrocytes (new)") |>
  dplyr::mutate(LBTEST = "Macrocytes (new 2)")

if (nrow(new_lbtest1) == 0 | nrow(new_lbtest2) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_lbtest1, new_lbtest2)

# Remove interim objects
rm(new_lbtest1, new_lbtest2)

# ADLB-VISITNUM-new_visitnum_01 -----------------------------------------------------

new_visitnum <- ADLB |>
  dplyr::filter(round(VISITNUM, 2) == 1.3) |>
  dplyr::mutate(VISITNUM = 1.4)

if (nrow(new_visitnum) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_visitnum)

# Remove interim objects
rm(new_visitnum)

# ADLB-VISITNUM-new_visitnum_02 -----------------------------------------------------

new_visitnum <- ADLB |>
  dplyr::filter(VISITNUM == 1.4) |>
  dplyr::mutate(VISITNUM = 1.5)

if (nrow(new_visitnum) == 0) {
  stop("No rows to add.")
}

ADLB <- rbind(ADLB, new_visitnum)

# Remove interim objects
rm(new_visitnum)


# Save ADLB ------------------------------------------------

cnt$adam$write_cnt(ADLB, "ADLB.parquet", overwrite = TRUE)

# Remove input tables
rm(list = c("ADSL", "lb", "sv"))
