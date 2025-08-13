test_that("Basic linear graph is handled correctly", {

  # SETUP -------------------------------------------------------------------
  edges <- data.frame(from = c("A", "B", "C"), to = c("B", "C", "D"))
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C", "D"),
    domain = c("ADSL", "ADAE", "ADCM", "ADLB")
  )

  # ACT ---------------------------------------------------------------------

  result <- sort_actions(edges, nodes)

  # EXPECT ------------------------------------------------------------------

  # Result should contain all 4 nodes
  expect_equal(length(result), 4)

  # Node A should be first as it has no incoming edges and is in primary domain
  expect_equal(result[1], "A")

  # Node D should be last as it has no outgoing edges
  expect_equal(result[4], "D")

  # Nodes B and C should be in the middle (either order is valid topologically)
  expect_true(all(c("B", "C") %in% result[2:3]))

  # Check topological order correctness
  for (i in 1:nrow(edges)) {
    from_idx <- which(result == edges$from[i])
    to_idx <- which(result == edges$to[i])
    # For each edge, the 'from' node should come before the 'to' node
    expect_true(from_idx < to_idx)
  }

})


test_that("Diamond-shaped graph is handled correctly", {

  # SETUP -------------------------------------------------------------------

  edges <- data.frame(
    from = c("A", "A", "B", "C"),
    to = c("B", "C", "D", "D")
  )
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C", "D"),
    domain = c("ADSL", "ADAE", "ADCM", "ADLB")
  )

  # ACT ---------------------------------------------------------------------

  result <- sort_actions(edges, nodes)

  # EXPECT ------------------------------------------------------------------

  # Result should contain all 4 nodes
  expect_equal(length(result), 4)

  # Node A should be first as it has no incoming edges and is in primary domain
  expect_equal(result[1], "A")

  # Node D should be last as it has no outgoing edges
  expect_equal(result[4], "D")

  # Nodes B and C should be in the middle (either order is valid topologically)
  expect_true(all(c("B", "C") %in% result[2:3]))

  # Check topological order correctness
  for (i in 1:nrow(edges)) {
    from_idx <- which(result == edges$from[i])
    to_idx <- which(result == edges$to[i])
    # For each edge, the 'from' node should come before the 'to' node
    expect_true(from_idx < to_idx)
  }
})


test_that("Topology is changed based on primary domain", {

  # SETUP -------------------------------------------------------------------

  # Graph with multiple valid topological orderings
  # A and B have no dependencies, C depends on both A and B
  # D depends on C, E depends on D
  edges <- data.frame(
    from = c("A", "B", "A", "B", "C", "D"),
    to = c("C", "C", "D", "D", "E", "E")
  )
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C", "D", "E"),
    domain = c("ADSL", "ADAE", "ADCM", "ADLB", "ADAE")
  )

  # ACT ---------------------------------------------------------------------

  # With ADSL as primary domain, A should be prioritized
  result1 <- sort_actions(edges, nodes, primary_domain = "ADSL")

  # With ADAE as primary domain, B and E should be prioritized
  result2 <- sort_actions(edges, nodes, primary_domain = "ADAE")

  # EXPECT ------------------------------------------------------------------

  # Both results should contain all 5 nodes
  expect_equal(length(result1), 5)
  expect_equal(length(result2), 5)

  # Check that topological order is maintained in both results
  for (i in 1:nrow(edges)) {
    from_idx1 <- which(result1 == edges$from[i])
    to_idx1 <- which(result1 == edges$to[i])

    # For each edge in result1, the 'from' node should come before the 'to' node
    expect_true(from_idx1 < to_idx1)

    from_idx2 <- which(result2 == edges$from[i])
    to_idx2 <- which(result2 == edges$to[i])

    # For each edge in result2, the 'from' node should come before the 'to' node
    expect_true(from_idx2 < to_idx2)
  }

  # In result1 (ADSL primary), A should come before B
  # Since A and B have no dependencies on each other, but A is in primary domain
  expect_true(which(result1 == "A") < which(result1 == "B"))

  # In result2 (ADAE primary), B should come before A
  # Since A and B have no dependencies on each other, but B is in primary domain
  expect_true(which(result2 == "B") < which(result2 == "A"))

  # Verify that the topological orderings are different
  expect_false(identical(result1, result2))

  # Both results must maintain the core dependencies:
  # C must come after both A and B
  expect_true(which(result1 == "A") < which(result1 == "C"))
  expect_true(which(result1 == "B") < which(result1 == "C"))
  expect_true(which(result2 == "A") < which(result2 == "C"))
  expect_true(which(result2 == "B") < which(result2 == "C"))

  # D must come after C
  expect_true(which(result1 == "C") < which(result1 == "D"))
  expect_true(which(result2 == "C") < which(result2 == "D"))

  # E must come after D
  expect_true(which(result1 == "D") < which(result1 == "E"))
  expect_true(which(result2 == "D") < which(result2 == "E"))
})


test_that("Cycles are detected", {

  # SETUP -------------------------------------------------------------------

  # Create a graph with a cycle: A -> B -> C -> D -> B
  # This forms a cycle because B depends on A, C depends on B,
  # D depends on C, and B depends on D (creating a circular dependency)
  edges <-  data.frame(
    from = c("A", "B", "C", "D"),
    to = c("B", "C", "D", "B")  # D -> B creates the cycle
  )
  nodes <-  data.table::data.table(
    node_id = c("A", "B", "C", "D"),
    domain = c("ADSL", "ADAE", "ADCM", "ADLB")
  )

  # ACT & EXPECT ------------------------------------------------------------

  # Topological sorting is only possible on directed acyclic graphs (DAGs)
  # Since our graph contains a cycle, the function should detect this and throw an error
  expect_error(
    sort_actions(edges, nodes),
    "The graph contains a cycle"
  )

  # Test with a different primary domain to ensure cycle detection works regardless of domain weights
  expect_error(
    sort_actions(edges, nodes, primary_domain = "ADCM"),
    "The graph contains a cycle"
  )
})


test_that("Disconnected components afre handled correctly", {

  # SETUP -------------------------------------------------------------------

  # Create a graph with two disconnected components:
  # Component 1: A -> B -> C
  # Component 2: D -> E -> F
  edges <-  data.frame(
    from = c("A", "B", "D", "E"),
    to = c("B", "C", "E", "F")
  )
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C", "D", "E", "F"),
    domain = c("ADSL", "ADAE", "ADCM", "ADSL", "ADLB", "ADAE")
  )

  # ACT ---------------------------------------------------------------------

  # Default primary domain is ADSL
  result <- sort_actions(edges, nodes)

  # EXPECT ------------------------------------------------------------------

  # Result should contain all 6 nodes
  expect_equal(length(result), 6)

  # Check topological order correctness

  # A must come before B
  expect_true(which(result == "A") < which(result == "B"))

  # B must come before C
  expect_true(which(result == "B") < which(result == "C"))

  # D must come before E
  expect_true(which(result == "D") < which(result == "E"))

    # E must come before F
  expect_true(which(result == "E") < which(result == "F"))

})


test_that("Non-existent primary domain is handled correctly", {

  # SETUP -------------------------------------------------------------------

  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C"),
    domain = c("DOM1", "DOM2", "DOM3")
  )

  # ACT ---------------------------------------------------------------------

  result <- sort_actions(edges, nodes, primary_domain = "NONEXISTENT")

  # EXPECT ------------------------------------------------------------------

  # Order should be unaffected by primary domain
  expect_equal(result, c("A", "B", "C"))
})


test_that("Isolated nodes are handled correctly", {

  # SETUP -------------------------------------------------------------------

  edges <- data.frame(from = c("A", "B"), to = c("B", "C"))
  nodes <- data.table::data.table(
    node_id = c("A", "B", "C", "D", "E"),  # D and E are isolated
    domain = c("ADSL", "ADAE", "ADCM", "ADLB", "ADSL")
  )

  # ACT ---------------------------------------------------------------------

  result <- sort_actions(edges, nodes)

  # EXPECT ------------------------------------------------------------------

  # Result should contain all 5 nodes
  expect_equal(length(result), 5)

  # A and E are both in primary domain (ADSL) A has no incoming edges in the
  # graph E is isolated (no edges at all). Both should have weight 1 and be
  # prioritized, but their relative order depends on implementation details of
  # the adjacency matrix creation
  expect_true("E" %in% result[1:2])  # E is in ADSL and isolated
})
