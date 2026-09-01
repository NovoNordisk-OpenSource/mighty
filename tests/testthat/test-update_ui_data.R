test_that("classify_action_type treats a single base domain as local (col_rename)", {
  actual <- classify_action_type(
    code_id = NA,
    type_from_code = NA,
    depend_cols = list("LB.LBSEQ"),
    outputs = list("SRCSEQ"),
    domain = "ADLB",
    base_domains = list(ADLB = "LB")
  )

  expect_equal(actual, "col_rename")
})

test_that("classify_action_type does not treat a second base domain as local (col_echo)", {
  # Guards against sweeping multi-base-domain harmonization (e.g. LB and XL
  # both feeding ADLB with differently-named sequence vars) into col_rename -
  # that case must go through a custom col_compute component instead.
  actual <- classify_action_type(
    code_id = NA,
    type_from_code = NA,
    depend_cols = list("XL.SOMECOL"),
    outputs = list("SOMECOL"),
    domain = "ADLB",
    base_domains = list(ADLB = c("LB", "XL"))
  )

  expect_equal(actual, "col_echo")
})
