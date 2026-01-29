test_that("assert_valid_edges validates input parameters", {
  # Valid test data
  valid_edges <- data.table::data.table(
    parent_node = c("ADSL-init_domain", "ADSL-init_domain"),
    node_id = c("ADSL-derive_age", "ADSL-derive_sex")
  )

  valid_nodes <- data.table::data.table(
    node_id = c("ADSL-init_domain", "ADSL-derive_age", "ADSL-derive_sex"),
    domain = c("ADSL", "ADSL", "ADSL"),
    type = c("init_domain", "col_mutate", "col_mutate")
  )

  # Test NULL inputs
  expect_error(
    assert_valid_edges(NULL, valid_nodes),
    "Must be a data.table"
  )

  expect_error(
    assert_valid_edges(valid_edges, NULL),
    "Must be a data.table"
  )

  # Test missing required columns in edges
  bad_edges <- data.table::data.table(
    wrong_parent = c("ADSL-init_domain"),
    node_id = c("ADSL-derive_age")
  )

  expect_error(
    assert_valid_edges(bad_edges, valid_nodes),
    "edges column names.*must include.*parent_node"
  )

  # Test missing required columns in nodes
  bad_nodes <- data.table::data.table(
    node_id = c("ADSL-init_domain"),
    domain = c("ADSL")
  )

  expect_error(
    assert_valid_edges(valid_edges, bad_nodes),
    "nodes column names.*must include.*domain.*type"
  )
})

test_that("assert_valid_edges catches empty edges table", {
  empty_edges <- data.table::data.table(
    parent_node = character(0),
    node_id = character(0)
  )

  many_nodes <- data.table::data.table(
    node_id = c(
      "ADSL-init_domain",
      "ADSL-derive_age",
      "ADSL-derive_sex",
      "ADSL-compute_bmi"
    ),
    domain = c("ADSL", "ADSL", "ADSL", "ADSL"),
    type = c("init_domain", "col_mutate", "col_mutate", "col_compute"),
    outputs = list(c("USUBJID"), c("AGE"), c("SEX"), c("BMI")),
    depend_cols = list(
      data.frame(column_name = character(0)),
      data.frame(column_name = character(0)),
      data.frame(column_name = character(0)),
      data.frame(column_name = character(0))
    )
  )

  expect_error(
    assert_valid_edges(empty_edges, many_nodes),
    "3 Dependency Inconsistencies Found"
  )

  no_init_nodes <- data.table::data.table(
    node_id = c("ADSL-derive_age", "ADSL-derive_sex"),
    domain = c("ADSL", "ADSL"),
    type = c("col_mutate", "col_mutate"),
    outputs = list(c("AGE"), c("SEX")),
    depend_cols = list(
      data.frame(column_name = character(0)),
      data.frame(column_name = character(0))
    )
  )

  # Error when no init_domain actions are present
  err_msg <- expect_error(
    assert_valid_edges(empty_edges, no_init_nodes),
    "Dependency Inconsistencies Found"
  )

  expect_match(
    err_msg$body,
    "The `init_domain` node is missing for .*ADSL"
  )
})

test_that("assert_valid_edges validates connectivity to init_domain parents", {
  # Test case: All nodes properly connected to init_domain
  valid_edges <- data.table::data.table(
    parent_node = c("ADSL-init_domain", "ADSL-init_domain", "ADSL-derive_age"),
    node_id = c("ADSL-derive_age", "ADSL-derive_sex", "ADSL-derive_bmi")
  )

  valid_nodes <- data.table::data.table(
    node_id = c(
      "ADSL-init_domain",
      "ADSL-derive_age",
      "ADSL-derive_sex",
      "ADSL-derive_bmi"
    ),
    domain = c("ADSL", "ADSL", "ADSL", "ADSL"),
    type = c("init_domain", "col_mutate", "col_mutate", "col_compute")
  )

  # Should not error
  expect_invisible(assert_valid_edges(valid_edges, valid_nodes))

  # Test case: Node not connected to any init_domain
  bad_edges <- data.table::data.table(
    parent_node = c("ADSL-init_domain", "ADSL-orphan_parent"),
    node_id = c("ADSL-derive_age", "ADSL-orphan_child")
  )

  bad_nodes <- data.table::data.table(
    node_id = c(
      "ADSL-init_domain",
      "ADSL-derive_age",
      "ADSL-orphan_parent",
      "ADSL-orphan_child"
    ),
    domain = c("ADSL", "ADSL", "ADSL", "ADSL"),
    type = c("init_domain", "col_mutate", "col_mutate", "col_mutate")
  )

  err_msg <- expect_error(
    assert_valid_edges(bad_edges, bad_nodes),
    "2 Dependency Inconsistencies Found"
  )

  expect_match(
    err_msg$body[3],
    "ADSL-orphan_parent"
  )
  expect_match(
    err_msg$body[4],
    "ADSL-orphan_child"
  )
})

test_that("assert_valid_edges handles multi-domain scenarios", {
  # Test with multiple domains
  multi_domain_edges <- data.table::data.table(
    parent_node = c(
      "ADSL-init_domain",
      "ADSL-init_domain",
      "ADAE-init_domain",
      "ADAE-init_domain"
    ),
    node_id = c(
      "ADSL-derive_age",
      "ADSL-derive_sex",
      "ADAE-derive_severity",
      "ADAE-derive_onset"
    )
  )

  multi_domain_nodes <- data.table::data.table(
    node_id = c(
      "ADSL-init_domain",
      "ADSL-derive_age",
      "ADSL-derive_sex",
      "ADAE-init_domain",
      "ADAE-derive_severity",
      "ADAE-derive_onset"
    ),
    domain = c("ADSL", "ADSL", "ADSL", "ADAE", "ADAE", "ADAE"),
    type = c(
      "init_domain",
      "col_mutate",
      "col_mutate",
      "init_domain",
      "col_mutate",
      "col_mutate"
    )
  )

  # Should not error
  expect_invisible(assert_valid_edges(multi_domain_edges, multi_domain_nodes))

  # Test with unconnected node in one domain
  bad_multi_domain_edges <- data.table::data.table(
    parent_node = c("ADSL-init_domain", "ADAE-init_domain", "ADAE-orphan"),
    node_id = c("ADSL-derive_age", "ADAE-derive_severity", "ADAE-orphan_child")
  )

  bad_multi_domain_nodes <- data.table::data.table(
    node_id = c(
      "ADSL-init_domain",
      "ADSL-derive_age",
      "ADAE-init_domain",
      "ADAE-derive_severity",
      "ADAE-orphan",
      "ADAE-orphan_child"
    ),
    domain = c("ADSL", "ADSL", "ADAE", "ADAE", "ADAE", "ADAE"),
    type = c(
      "init_domain",
      "col_mutate",
      "init_domain",
      "col_mutate",
      "col_mutate",
      "col_mutate"
    )
  )

  expect_error(
    assert_valid_edges(bad_multi_domain_edges, bad_multi_domain_nodes),
    "2 Dependency Inconsistencies Found"
  )
})

test_that("assert_valid_edges returns edges invisibly on success", {
  valid_edges <- data.table::data.table(
    parent_node = c("ADSL-init_domain", "ADSL-init_domain"),
    node_id = c("ADSL-derive_age", "ADSL-derive_sex")
  )

  valid_nodes <- data.table::data.table(
    node_id = c("ADSL-init_domain", "ADSL-derive_age", "ADSL-derive_sex"),
    domain = c("ADSL", "ADSL", "ADSL"),
    type = c("init_domain", "col_mutate", "col_mutate")
  )

  result <- assert_valid_edges(valid_edges, valid_nodes)
  expect_identical(result, valid_edges)
})
