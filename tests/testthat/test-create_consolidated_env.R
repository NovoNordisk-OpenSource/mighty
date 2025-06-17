test_that("create_consolidated_env errors on non-existent file", {
  # Specify a non-existent file path
  non_existent_file <- "path/to/non_existent_file.R"
  create_consolidated_env(
      packages = NULL,
      source_files = c(non_existent_file),
      code_ids = c("fn_1")
    ) |>
    expect_error(regexp = "Error: File 'path/to/non_existent_file.R' does not exist.")
})


test_that("create_consolidated_env errors on non-existent package", {
  # Specify a non-existent package name
  non_existent_package <- "nonexistentpkg123"

  # Expect the function to stop with an error message
  expect_error(
    create_consolidated_env(
      packages = c(non_existent_package),
      source_files = NULL,
      code_ids = c("fn_1")
    ),
    # Match the expected error message
    regexp = "Error: Package 'nonexistentpkg123' is not available."
  )
})

test_that("create_consolidated_env handles code_ids not found in any source", {
  # ARRANGE -------------------------------------------------------------------
  # Create a temporary R file with a function that doesn't match our code_ids
  temp_file <- tempfile(fileext = ".R")
  writeLines("existing_function <- function() { return('I exist') }", temp_file)

  # Use a real package (base) and specify code_ids that don't exist anywhere
  packages <- c("base", "stats")
  source_files <- temp_file
  code_ids <- c("existing_function", "nonexistent_function", "another_missing_function", "paste", "AIC")

  # ACT & ASSERT ---------------------------------------------------------------
  create_consolidated_env(
    packages = packages,
    source_files = source_files,
    code_ids = code_ids
  ) |> expect_error("The following code_ids were not found in any source")

  create_consolidated_env(
    packages = packages,
    source_files = source_files,
    code_ids = code_ids
  ) |> expect_error("nonexistent_function")

  create_consolidated_env(
    packages = packages,
    source_files = source_files,
    code_ids = code_ids
  ) |> expect_error("another_missing_function")

  # Expect an error that does NOT contain "paste" in the missing functions list
  expect_error(
    create_consolidated_env(
      packages = packages,
      source_files = source_files,
      code_ids = code_ids
    ),
    regexp = "^(?!.*'paste').*The following code_ids were not found.*",
    perl = TRUE,
    info = "Error message should not include 'paste' as it exists in base package"
  )
  # CLEANUP -------------------------------------------------------------------
  unlink(temp_file)
})


testthat::test_that("create_consolidated_env detects duplicate function names from files",
                    {
                      # Create temporary files for testing
                      temp_file_1 <- withr::local_tempfile(fileext = ".R")
                      temp_file_2 <- withr::local_tempfile(fileext = ".R")
                      temp_file_3 <- withr::local_tempfile(fileext = ".R")

                      # Write mock functions to temp_file_1
                      writeLines(
                        c(
                          "test_function <- function() {",
                          "  return('Hello from file 1')",
                          "}",
                          "non_duplicate_fn <- function() {",
                          "  return('Hello from file 1')",
                          "}"
                        ),
                        temp_file_1
                      )

                      # Write a conflicting mock function to temp_file_2
                      writeLines(
                        c(
                          "test_function <- function() {",
                          "  return('Hello from file 2')",
                          "}",
                          "dup_2 <- function() {",
                          "  return('Hello from file 2')",
                          "}"
                        ),
                        temp_file_2
                      )

                      writeLines(
                        c(
                          "test_function <- function() {",
                          "  return('Hello from file 3')",
                          "}",
                          "dup_2 <- function() {",
                          "  return('Hello from file 2')",
                          "}"
                        ),
                        temp_file_3
                      )

                      # Expect an error due to duplicate function names
                      create_consolidated_env(
                        packages = NULL,
                        source_files = c(temp_file_1, temp_file_2, temp_file_3),
                        code_ids = c("test_function", "dup_2")
                      ) |>
                        expect_error(regexp = "Duplicate function 'dup_2' found in the following sources")
                      create_consolidated_env(
                        packages = NULL,
                        source_files = c(temp_file_1, temp_file_2, temp_file_3),
                        code_ids = c("test_function", "dup_2")
                      ) |>
                        expect_error(regexp = "Duplicate function 'test_function' found in the following sources")
                    })

test_that("create_consolidated_env loads functions from both packages and files",
          {
            # Create a temporary package
            withr::with_tempdir(code = {
              pkg_name <- "temppkg"
              usethis::create_package(
                path = pkg_name,
                fields = list(
                  Title = "tmp",
                  Description = "tmp",
                  Version = "0.1.0"
                ),
                open = FALSE,
                rstudio = FALSE
              )

              # Add functions to the package
              func_path <- file.path(pkg_name, "R", "functions.R")
              dir.create(file.path(pkg_name, "R"),
                         recursive = TRUE,
                         showWarnings = FALSE)

              writeLines(
                c(
                  "#' Package Function 1",
                  "#' @export",
                  "pkg_function1 <- function() {",
                  "  return('Hello from package function 1')",
                  "}",
                  "",
                  "#' Package Function 2",
                  "#' @export",
                  "pkg_function2 <- function() {",
                  "  return('Hello from package function 2')",
                  "}"
                ),
                func_path
              )

              # Generate documentation and NAMESPACE
              withr::with_dir(pkg_name, {
                roxygen2::roxygenize(".")
              })

              # Create a temporary library directory
              lib_dir <- file.path(getwd(), "temp_lib")
              dir.create(lib_dir)

              # Create a temporary source file
              temp_file <- withr::local_tempfile(fileext = ".R")
              writeLines(
                c(
                  "file_function1 <- function() {",
                  "  return('Hello from file function 1')",
                  "}",
                  "",
                  "file_function2 <- function() {",
                  "  return('Hello from file function 2')",
                  "}"
                ),
                temp_file
              )

              # Install the package to the temporary library
              withr::with_libpaths(new = lib_dir, code = {
                devtools::install(pkg = pkg_name,
                                  quiet = TRUE,
                                  upgrade = "never")

                # Test the create_consolidated_env function
                code_ids <- c("pkg_function1",
                              "pkg_function2",
                              "file_function1",
                              "file_function2")

                # Create consolidated environment with functions from both package and file
                consolidated_env <- create_consolidated_env(
                  packages = "temppkg",
                  source_files = temp_file,
                  code_ids = code_ids
                )

                # Check that all functions exist in the consolidated environment
                expect_true(exists("pkg_function1", envir = consolidated_env))
                expect_true(exists("pkg_function2", envir = consolidated_env))
                expect_true(exists("file_function1", envir = consolidated_env))
                expect_true(exists("file_function2", envir = consolidated_env))

                # Check that functions return expected values
                expect_equal(consolidated_env$pkg_function1(),
                             "Hello from package function 1")
                expect_equal(consolidated_env$pkg_function2(),
                             "Hello from package function 2")
                expect_equal(consolidated_env$file_function1(),
                             "Hello from file function 1")
                expect_equal(consolidated_env$file_function2(),
                             "Hello from file function 2")
              })
            })
          })



test_that("create_consolidated_env detects duplicates between package and file",
          {
            # Create a temporary package with a function that will conflict
            withr::with_tempdir(code = {
              pkg_name <- "temppkg"
              usethis::create_package(
                path = pkg_name,
                fields = list(
                  Title = "Temporary Package for Testing",
                  Description = "A temporary package created for testing purposes.",
                  Version = "0.1.0"
                ),
                open = FALSE,
                rstudio = FALSE
              )

              # Add functions to the package
              func_path <- file.path(pkg_name, "R", "functions.R")
              dir.create(file.path(pkg_name, "R"),
                         recursive = TRUE,
                         showWarnings = FALSE)

              writeLines(
                c(
                  "#' Duplicate Function",
                  "#' @export",
                  "duplicate_function <- function() {",
                  "  return('Hello from package')",
                  "}",
                  "",
                  "#' Unique Package Function",
                  "#' @export",
                  "unique_pkg_function <- function() {",
                  "  return('Hello from package unique')",
                  "}"
                ),
                func_path
              )

              # Generate documentation and NAMESPACE
              withr::with_dir(pkg_name, {
                roxygen2::roxygenize(".")
              })

              # Create a temporary library directory
              lib_dir <- file.path(getwd(), "temp_lib")
              dir.create(lib_dir)

              # Create a temporary source file with a conflicting function name
              temp_file <- withr::local_tempfile(fileext = ".R")
              writeLines(
                c(
                  "duplicate_function <- function() {",
                  "  return('Hello from file')",
                  "}",
                  "",
                  "unique_file_function <- function() {",
                  "  return('Hello from file unique')",
                  "}"
                ),
                temp_file
              )

              # Install the package to the temporary library
              withr::with_libpaths(new = lib_dir, code = {
                devtools::install(pkg = pkg_name,
                                  quiet = TRUE,
                                  upgrade = "never")

                # Test the create_consolidated_env function
                code_ids <- c("duplicate_function",
                              "unique_pkg_function",
                              "unique_file_function")

                # Expect an error due to duplicate function names

                create_consolidated_env(
                  packages = "temppkg",
                  source_files = temp_file,
                  code_ids = code_ids
                ) |>
                  expect_error(regexp = "Duplicate function 'duplicate_function' found in the following sources")
              })
            })
          })
