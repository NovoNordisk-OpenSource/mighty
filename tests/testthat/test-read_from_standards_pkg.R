test_that("Can read code components from another package", {
  # SETUP
  ui_path <- c(test_path("fixtures", "parameters_with_code.yml"))
  path_trial_metadata <- test_path("fixtures", "trial_metadata_0001.yml")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  std_lib_path <- c(testthat::test_path("fixtures", "parameters_with_code.R"))
  output_path <- withr::local_tempdir()

  # Load from fixture to eventually write into tmp pkg or tmp dir
  ui_yml <- yaml::read_yaml(ui_path)
  trial_metadata_yml <- yaml::read_yaml(path_trial_metadata)
  fns <- readLines(std_lib_path)
  withr::with_tempdir(code = {
    pkg_name <-  "tmpcodepkg"
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

    # Create a temporary library directory
    lib_dir <- file.path(getwd(), "temp_lib")
    dir.create(lib_dir)

    # Add a function with roxygen documentation
    func_path <- file.path(pkg_name, "R", "functions.R")
    writeLines(
      fns,
      func_path
    )

    # Generate documentation
    withr::with_dir(pkg_name, {
      roxygen2::roxygenize(".")
    })
browser()
    withr::with_libpaths(
      new = lib_dir,
      code = {

        yaml::write_yaml(ui_yml, file="ui_yml.yml")
        yaml::write_yaml(trial_metadata_yml, file="trial_metadata_yml.yml")

        devtools::install(
          pkg = pkg_name,
          quiet = TRUE,
          upgrade = "never"
        )

        actual <- generate_adam_code(
          path_ui_data = "ui_yml.yml",
          code_component_source_pkgs = "tmpcodepkg",
          path_trial_metadata = "trial_metadata_yml.yml",
          path_domain_keys = domain_keys_path,
          path_output = ".",
          data_connection = "pharmaverse"
        )

        # Test that we can access the documentation
        expect_true(length(rd_db) > 0)
        expect_true("add_numbers.Rd" %in% names(rd_db))
      }
    )

    # No need for explicit cleanup - withr::with_tempdir handles it
  })
})
