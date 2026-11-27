make_program_actions <- function(domain, read_outputs, removed_outputs = NULL) {
  actions <- data.table(
    domain = domain,
    code_id = c("mighty_read_data", "mighty_init_domain"),
    outputs = list(read_outputs, character(0))
  )
  if (!is.null(removed_outputs)) {
    actions$removed_outputs <- list(removed_outputs, NA)
  }
  actions
}

fixed_time <- as.POSIXct("2026-11-27 13:03:12", tz = "UTC")

test_that("gen_program_header creates the full header", {
  local_mocked_bindings(mighty_version = function() "9.9.9")
  actions <- make_program_actions(
    "ADSL",
    c("DM.USUBJID", "DM.AGE", "VS.VSSTRESN")
  )

  expect_snapshot(cat(gen_program_header(
    actions,
    part = 1,
    n_parts = 1,
    generated_at = fixed_time
  )))
})

test_that("gen_program_header numbers parts of split domains", {
  actions <- make_program_actions("ADSL", c("ADSL.AGE", "ADAE.AEDECOD"))

  header <- gen_program_header(
    actions,
    part = 2,
    n_parts = 3,
    generated_at = fixed_time
  )

  expect_match(
    header,
    "# Description:   Creates the ADSL analysis dataset (part 2 of 3).",
    fixed = TRUE
  )
  expect_match(header, "# Input:         ADAE, ADSL", fixed = TRUE)
})

test_that("gen_program_header shows the generation time in UTC", {
  local_time <- as.POSIXct("2026-11-27 14:03:12", tz = "Europe/Copenhagen")
  actions <- make_program_actions("ADSL", "DM.AGE")

  header <- gen_program_header(actions, 1, 1, generated_at = local_time)

  expect_match(header, "# Generated:     2026-11-27T13:03:12Z", fixed = TRUE)
})

test_that("format_program_inputs lists sorted unique domains", {
  make_program_actions(
    "ADAE",
    c("EX.EXTRT", "AE.AETERM", "ADSL.AGE", "AE.AESEV")
  ) |>
    format_program_inputs() |>
    expect_equal("ADSL, AE, EX")
})

test_that("format_program_inputs returns None when nothing is read", {
  make_program_actions("ADSL", character(0)) |>
    format_program_inputs() |>
    expect_equal("None")
})

test_that("format_program_inputs marks only fully missing domains as not available", {
  actions <- make_program_actions(
    "ADAE",
    read_outputs = c("ADSL.AGE", "AE.AETERM"),
    removed_outputs = c("AE.AESER", "EX.EXTRT", "EX.EXDOSE")
  )

  format_program_inputs(actions) |>
    expect_equal("ADSL, AE, EX (not available)")
})

test_that("format_program_inputs handles read actions without removed outputs", {
  actions <- make_program_actions("ADAE", "AE.AETERM", removed_outputs = NA)

  format_program_inputs(actions) |>
    expect_equal("AE")
})

test_that("format_header_field wraps long values to 80 characters", {
  value <- paste(sprintf("DOMAIN%02d", 1:15), collapse = ", ")

  lines <- format_header_field("Input", value)

  expect_gt(length(lines), 1)
  expect_true(all(nchar(lines) <= 80))
  expect_true(all(startsWith(lines[-1], paste0("# ", strrep(" ", 15)))))
})

test_that("generate_adam_code adds headers with one shared generation time", {
  local_mocked_bindings(
    generation_time = function() fixed_time,
    mighty_version = function() "9.9.9"
  )
  adam_specifications <- setup_study_from_fixtures(
    fixtures = list(
      "adsl" = "complex_adsl.yml",
      "adlb" = "complex_adlb.yml",
      "_mighty" = "_mighty.yml"
    )
  )

  actual <- generate_adam_code(adam_specifications = adam_specifications)

  all_programs <- c(actual$programs, actual$executable_programs)
  for (program in all_programs) {
    expect_match(program, "^# \\*{78}\n# Description:")
    expect_match(program, "# Generated:     2026-11-27T13:03:12Z", fixed = TRUE)
    expect_match(program, "# Generator:     mighty v9.9.9", fixed = TRUE)
  }
  headers <- sub("\n\n.*", "", actual$programs)
  expect_snapshot(cat(headers, sep = "\n\n"))
})
