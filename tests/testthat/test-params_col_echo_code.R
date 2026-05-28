test_that("params_col_echo_code returns expected structure", {
  result <- params_col_echo_code(
    .self = "adlb",
    depend_cols = c("STUDYID", "USUBJID", "AGE"),
    depend_domains = c("adlb", "adsl"),
    outputs = "AGE",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"),
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_type(result, "list")
  expect_named(
    result,
    c(
      "self",
      "join_dataset",
      "select_expr",
      "by_vars",
      "needs_rename",
      "output_var",
      "var_to_add"
    )
  )
  expect_equal(result$self, "adlb")
  expect_equal(result$join_dataset, "adsl")
  expect_equal(result$output_var, "AGE")
  expect_equal(result$var_to_add, "AGE")
  expect_false(result$needs_rename)
  expect_match(result$select_expr, "STUDYID")
  expect_match(result$select_expr, "USUBJID")
  expect_match(result$select_expr, "AGE")
  expect_match(result$by_vars, '"STUDYID"')
  expect_match(result$by_vars, '"USUBJID"')
})

test_that("params_col_echo_code handles rename case", {
  result <- params_col_echo_code(
    .self = "adlb",
    depend_cols = c("STUDYID", "USUBJID", "AAGE"),
    depend_domains = c("adlb", "adsl"),
    outputs = "AGE",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"),
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_equal(result$output_var, "AGE")
  expect_equal(result$var_to_add, "AAGE")
  expect_true(result$needs_rename)
})

test_that("pre_process_generate_rename_left_join_code errors on missing domain keys", {
  err <- expect_error(
    pre_process_generate_rename_left_join_code(
      depend_columns = c("STUDYID", "USUBJID", "AGE"),
      depend_domains = c("adlb", "adsl"),
      outputs = "AGE",
      domain = "adlb",
      domain_keys = list(
        ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
        # ADSL is missing
      )
    ),
    class = "rlang_error"
  )

  msg <- conditionMessage(err)
  expect_match(msg, "\\[Missing domain keys\\]")
  expect_match(msg, "adsl")
  expect_match(msg, "not defined")
  expect_match(msg, "Suggestions:")
  expect_match(msg, "_mighty.yml")
})

test_that("pre_process_generate_rename_left_join_code returns correct structure", {
  result <- pre_process_generate_rename_left_join_code(
    depend_columns = c("STUDYID", "USUBJID", "AGE"),
    depend_domains = c("adlb", "adsl"),
    outputs = "AGE",
    domain = "adlb",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"),
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_type(result, "list")
  expect_named(result, c("join_dataset", "var_to_add", "by_vars", "output_var"))
  expect_equal(result$join_dataset, "adsl")
  expect_equal(result$by_vars, c("STUDYID", "USUBJID"))
  expect_equal(result$var_to_add, "AGE")
  expect_equal(result$output_var, "AGE")
})

test_that("pre_process_generate_rename_left_join_code handles duplicate domains", {
  result <- pre_process_generate_rename_left_join_code(
    depend_columns = c("STUDYID", "USUBJID", "AGE"),
    depend_domains = c("adlb", "adsl", "adsl"), # duplicate ADSL
    outputs = "AGE",
    domain = "adlb",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"),
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_equal(result$join_dataset, "adsl")
})

test_that("pre_process_generate_rename_left_join_code handles domain-prefixed outputs", {
  result <- pre_process_generate_rename_left_join_code(
    depend_columns = c("STUDYID", "USUBJID", "AAGE"),
    depend_domains = c("adlb", "adsl"),
    outputs = "adlb.AGE",
    domain = "adlb",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"),
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_equal(result$var_to_add, "AAGE")
  expect_equal(result$output_var, "AGE")
})

test_that("pre_process_generate_rename_left_join_code handles case insensitive domain keys", {
  result <- pre_process_generate_rename_left_join_code(
    depend_columns = c("STUDYID", "USUBJID", "AGE"),
    depend_domains = c("adlb", "adsl"),
    outputs = "AGE",
    domain = "adlb",
    domain_keys = list(
      ADSL = c("STUDYID", "USUBJID"), # uppercase key name
      ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
    )
  )

  expect_equal(result$join_dataset, "adsl")
  expect_equal(result$by_vars, c("STUDYID", "USUBJID"))
})

test_that("pre_process_generate_rename_left_join_code errors with multiple join datasets", {
  expect_error(
    pre_process_generate_rename_left_join_code(
      depend_columns = c("AGE", "AEDECOD"),
      depend_domains = c("adlb", "adsl", "adae"), # two external domains
      outputs = "AGE",
      domain = "adlb",
      domain_keys = list(
        ADSL = c("STUDYID", "USUBJID"),
        ADAE = c("STUDYID", "USUBJID", "AESEQ"),
        ADLB = c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
      )
    ),
    "Assertion on"
  )
})
