test_that("load_functions_from_packages loads single function from single package successfully", {
  # ARRANGE -------------------------------------------------------------------
  packages <- "base"
  code_ids <- "paste"
  # Initialize function_sources as done in create_consolidated_env
  function_sources <- list(pkg = list(), file = list())
  # Use same environment pattern as calling function
  envr <- new.env(parent = emptyenv())

  # ACT -----------------------------------------------------------------------
  result <- load_functions_from_packages(packages, code_ids, function_sources, envr)

  # ASSERT --------------------------------------------------------------------
  # Function should be loaded into the target environment
  expect_true(exists("paste", envir = envr),
              info = "Function should be assigned to target environment for downstream consolidation")

  # Loaded function should actually be a function
  expect_true(is.function(get("paste", envir = envr)),
              info = "Loaded object should be a function to ensure proper consolidation")

  # Loaded function should be the same as the original from base package
  expect_identical(get("paste", envir = envr), base::paste,
                   info = "Loaded function should match original to maintain expected behavior")

  # Return value should be the pkg portion of function_sources for assignment back
  expect_true(is.list(result),
              info = "Return value should be list structure for assignment to function_sources$pkg")

  # Return value should have the package name as a key for duplicate checking
  expect_true("base" %in% names(result),
              info = "Return value should contain package entry for duplicate detection")

  # Package entry should contain the function information for tracking
  expect_true("paste" %in% names(result[["base"]]),
              info = "Package entry should contain function name for source tracking")

  # Function source information should be correctly recorded for duplicate detection
  expect_equal(result[["base"]][["paste"]][["fn"]], "paste",
               info = "Function name should be recorded for duplicate checking across sources")

  expect_equal(result[["base"]][["paste"]][["fn_source"]], "package base",
               info = "Function source should identify package origin for duplicate error messages")
})

test_that("load_functions_from_packages loads multiple functions from single package successfully", {
  # ARRANGE -------------------------------------------------------------------
  packages <- "base"
  code_ids <- c("paste", "length")
  function_sources <- list(pkg = list(), file = list())
  envr <- new.env(parent = emptyenv())

  # ACT -----------------------------------------------------------------------
  result <- load_functions_from_packages(packages, code_ids, function_sources, envr)

  # ASSERT --------------------------------------------------------------------
  # Both functions should be loaded into the target environment
  expect_true(exists("paste", envir = envr),
              info = "First function should be assigned to target environment")
  expect_true(exists("length", envir = envr),
              info = "Second function should be assigned to target environment")
  # Loaded functions should match originals from base package
  expect_identical(get("paste", envir = envr), base::paste,
                   info = "First function should match original from package")
  expect_identical(get("length", envir = envr), base::length,
                   info = "Second function should match original from package")

  # Return value should contain single package entry
  expect_equal(length(result), 1,
               info = "Return value should contain exactly one package entry")
  expect_true("base" %in% names(result),
              info = "Return value should contain the loaded package")

  # Package entry should contain both functions
  expect_equal(length(result[["base"]]), 2,
               info = "Package entry should contain both loaded functions")
  expect_true(all(c("paste", "length") %in% names(result[["base"]])),
              info = "Package entry should contain both requested function names")

  # Each function should have correct source information
  expect_equal(result[["base"]][["paste"]][["fn"]], "paste",
               info = "First function name should be correctly recorded")
  expect_equal(result[["base"]][["paste"]][["fn_source"]], "package base",
               info = "First function source should be correctly recorded")

  expect_equal(result[["base"]][["length"]][["fn"]], "length",
               info = "Second function name should be correctly recorded")
  expect_equal(result[["base"]][["length"]][["fn_source"]], "package base",
               info = "Second function source should be correctly recorded")

})

test_that("load_functions_from_packages ignores code_ids not found in package(s)", {
  # ARRANGE -------------------------------------------------------------------
  packages <- "base"
  code_ids <- c("paste", "nonexistent_function_xyz")
  function_sources <- list(pkg = list(), file = list())
  envr <- new.env(parent = emptyenv())
  # ACT -----------------------------------------------------------------------
  result <- load_functions_from_packages(packages, code_ids, function_sources, envr)

  # ASSERT --------------------------------------------------------------------
  # Only existing function should be loaded
  expect_true(exists("paste", envir = envr),
              info = "Existing function should be loaded despite non-existent function in list")
  expect_false(exists("nonexistent_function_xyz", envir = envr),
               info = "Non-existent function should not be loaded")

  # Return value should contain only the existing function
  expect_equal(length(result[["base"]]), 1,
               info = "Only existing function should be tracked in return value")
  expect_true("paste" %in% names(result[["base"]]),
              info = "Existing function should be present in tracking")
  expect_false("nonexistent_function_xyz" %in% names(result[["base"]]),
               info = "Non-existent function should not be tracked")
})
