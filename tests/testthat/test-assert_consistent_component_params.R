# Helper to build a minimal data.table matching the structure expected by
# assert_consistent_component_params().
make_component_dt <- function(rows) {
  data.table::data.table(
    domain = vapply(rows, `[[`, character(1), "domain"),
    code_id = vapply(rows, `[[`, character(1), "code_id"),
    type_from_code = vapply(rows, `[[`, character(1), "type_from_code"),
    outputs_from_code = lapply(rows, `[[`, "outputs_from_code"),
    parameters = lapply(rows, `[[`, "parameters")
  )
}

# -- assert_consistent_component_params ----------------------------------------

test_that("passes when no derivation components are present", {
  dt <- make_component_dt(list(
    list(
      domain = "ADSL",
      code_id = "pred",
      type_from_code = "predecessor",
      outputs_from_code = "X",
      parameters = list()
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})

test_that("passes when all code_id values are NA", {
  dt <- data.table::data.table(
    domain = "ADSL",
    code_id = NA_character_,
    type_from_code = "derivation",
    outputs_from_code = list("A"),
    parameters = list(list())
  )
  expect_invisible(assert_consistent_component_params(dt))
})

test_that("passes with a single parameter set", {
  dt <- make_component_dt(list(
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "1")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})

test_that("passes when different parameter sets have disjoint outputs", {
  dt <- make_component_dt(list(
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "A",
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "B",
      parameters = list(p = "2")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("D", "C"),
      parameters = list(p = "2")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})

# Regression: find_overlapping_outputs() doesn't deduplicate within a parameter
# group, so repeated rows with the same params (e.g. c("A","B","A","B")) are
# falsely flagged as cross-group overlaps when a second parameter set exists.
test_that("repeated rows within a param group don't false-positive with a disjoint param set", {
  dt <- make_component_dt(list(
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("D", "C"),
      parameters = list(p = "2")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})

test_that("errors when overlapping outputs have different parameters", {
  dt <- make_component_dt(list(
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = c("A", "B"),
      parameters = list(p = "2")
    )
  ))
  expect_error(
    assert_consistent_component_params(dt),
    regexp = "different parameter values"
  )
})

test_that("error message mentions every affected domain", {
  dt <- make_component_dt(list(
    list(
      domain = "ADSL",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "X",
      parameters = list(p = "a")
    ),
    list(
      domain = "ADSL",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "X",
      parameters = list(p = "b")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "X",
      parameters = list(p = "a")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "X",
      parameters = list(p = "b")
    )
  ))
  expect_error(
    assert_consistent_component_params(dt),
    regexp = "ADSL.*ADLB|ADLB.*ADSL"
  )
})

test_that("different components in the same domain are checked independently", {
  dt <- make_component_dt(list(
    list(
      domain = "ADSL",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "A",
      parameters = list(p = "1")
    ),
    list(
      domain = "ADSL",
      code_id = "comp2",
      type_from_code = "derivation",
      outputs_from_code = "B",
      parameters = list(p = "2")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})
test_that("same component and parameters across different domains is allowed", {
  dt <- make_component_dt(list(
    list(
      domain = "ADSL",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "AGE",
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "AGE",
      parameters = list(p = "1")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})

test_that("same component with different parameters across domains is allowed even with same outputs", {
  dt <- make_component_dt(list(
    list(
      domain = "ADSL",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "AGE",
      parameters = list(p = "1")
    ),
    list(
      domain = "ADLB",
      code_id = "comp1",
      type_from_code = "derivation",
      outputs_from_code = "AGE",
      parameters = list(p = "2")
    )
  ))
  expect_invisible(assert_consistent_component_params(dt))
})
# -- display_component_id -----------------------------------------------------

test_that("display_component_id returns basename for file paths", {
  expect_equal(display_component_id("path/to/der_complsfl.R"), "der_complsfl.R")
})

test_that("display_component_id returns standard names as-is", {
  expect_equal(display_component_id("ady"), "ady")
})
