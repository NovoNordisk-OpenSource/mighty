test_that("Multiple interacting domains are grouped correctly", {
  # SETUP -------------------------------------------------------------------

  edges <- data.table(
    parent_node = c(
      "A1",
      "A2",
      "A3",
      "A4",
      "B1",
      "A2",
      "D1",
      "A3",
      "C1",
      "C2",
      "D2",
      "A3",
      "K1",
      "K2",
      "F1",
      "G1",
      "G1",
      "H1",
      "H2",
      "I1",
      "A5",
      "J1",
      "I2",
      "E1",
      "L1"
    ),
    node_id = c(
      "A2",
      "A3",
      "A4",
      "A5",
      "B2",
      "B2",
      "D2",
      "D2",
      "C2",
      "C3",
      "C2",
      "K1",
      "K2",
      "A4",
      "F2",
      "G2",
      "H1",
      "H2",
      "G2",
      "I2",
      "I2",
      "J2",
      "J2",
      "A5",
      "H1"
    )
  )

  actions <- data.table(
    node_id = sort(unique(c(edges$parent_node, edges$node_id))),
    type = "not_col_copy"
  )
  actions[["domain"]] <- substr(actions$node_id, 1, 1)

  actions$domain[actions$domain == "A"] <- "ADSL"

  # ACT ---------------------------------------------------------------------

  result <- organize_actions(actions, edges)

  # EXPECT ------------------------------------------------------------------

  expect_equal(
    result$node_id,
    c(
      "A1",
      "A2",
      "A3",
      "E1",
      "K1",
      "K2",
      "A4",
      "A5",
      "B1",
      "B2",
      "D1",
      "D2",
      "I1",
      "I2",
      "C1",
      "C2",
      "C3",
      "J1",
      "J2",
      "F1",
      "F2",
      "L1",
      "G1",
      "H1",
      "H2",
      "G2"
    )
  )

  expect_equal(max(result$program_id), 14)
})


test_that("Test that selection of primary domain supersedes minimizing the number of programs", {
  # SETUP -------------------------------------------------------------------

  edges <- data.table(
    parent_node = c(
      "A1",
      "A2",
      "A3",
      "A4",
      "B1",
      "A2",
      "D1",
      "A3",
      "C1",
      "C2",
      "D2",
      "A3",
      "K1",
      "K2",
      "F1",
      "G1",
      "G1",
      "H1",
      "H2",
      "I1",
      "A5",
      "J1",
      "I2",
      "E1",
      "L1"
    ),
    node_id = c(
      "A2",
      "A3",
      "A4",
      "A5",
      "B2",
      "B2",
      "D2",
      "D2",
      "C2",
      "C3",
      "C2",
      "K1",
      "K2",
      "A4",
      "F2",
      "G2",
      "H1",
      "H2",
      "G2",
      "I2",
      "I2",
      "J2",
      "J2",
      "A5",
      "H1"
    )
  )

  actions <- data.table(
    node_id = sort(unique(c(edges$parent_node, edges$node_id))),
    type = "not_col_copy"
  )
  actions[["domain"]] <- substr(actions$node_id, 1, 1)

  actions$domain[actions$domain == "B"] <- "ADSL"

  # ACT ---------------------------------------------------------------------

  result <- organize_actions(actions, edges)

  # EXPECT ------------------------------------------------------------------

  expect_equal(max(result$program_id), 15)
})
