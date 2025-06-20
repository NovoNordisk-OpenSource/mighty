test_that("Can read code components from another package", {
  # SETUP
  withr::defer({
    # This is needed to ensure the temporary library is completely cleaned up
    if (paste0("package:", "tmpcodepkg") %in% search()) {
      try(detach(paste0("package:", "tmpcodepkg"), unload = TRUE), silent = TRUE)
    }
    if ("tmpcodepkg" %in% loadedNamespaces()) {
      try(unloadNamespace("tmpcodepkg"), silent = TRUE)
    }
    gc()
  })
  ui_path <- c(testthat::test_path("fixtures", "parameters_with_code.yml"))
  path_trial_metadata <- testthat::test_path("fixtures", "trial_metadata_0001.yml")
  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  std_lib_path <- c(testthat::test_path("fixtures", "parse_from_pkg.R"))
  output_path <- withr::local_tempdir()
  setup_testdata(testdata = "pharmaverse", test_data_path = output_path)
  .connector <- yaml::read_yaml(file.path(output_path, "_connector.yml"), eval.expr = FALSE)
  yaml::write_yaml(.connector, file.path(output_path, "_connector.yml"))
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
    lib_dir <- fs::path_wd("temp_lib")
    dir.create(lib_dir)

    # Add a function with roxygen documentation
    func_path <- file.path(pkg_name, "R", "functions.R")
    writeLines(fns, func_path)

    # Generate documentation
    withr::with_dir(pkg_name, {
      roxygen2::roxygenize(".")
    })

    # Run code to test with tempory library
    withr::with_libpaths(new = lib_dir, code = {
      yaml::write_yaml(ui_yml, file = "ui_yml.yml")
      yaml::write_yaml(trial_metadata_yml, file = "trial_metadata_yml.yml")

      devtools::install(pkg = pkg_name,
                        quiet = TRUE,
                        upgrade = "never")

      file.copy(
        from = file.path(output_path, "_connector.yml"),
        to = "_connector.yml",
        overwrite = TRUE
      )
      fs::dir_copy(path=file.path(output_path, "data"),
                   new_path = "data",
                   overwrite = TRUE)

      actual <- generate_adam_code(
        path_ui_data = "ui_yml.yml",
        code_component_source_pkgs = "tmpcodepkg",
        path_trial_metadata = "trial_metadata_yml.yml",
        path_output = "."
      )

      # Function with params
      expect_snapshot_value(actual$programs$`1_ADSL`[[3]], style = "json2")

      # Function with mixed default parameter values and user-supplied values
      expect_snapshot_value(actual$programs$`1_ADSL`[[4]], style = "json2")

      # Function with no parameters
      expect_snapshot_value(actual$programs$`1_ADSL`[[5]], style = "json2")

      # Specific un-evaluated user-supplied parameter
      prog <- actual$programs$`1_ADSL` |> unlist()
      expect_equal(1, grepl("param_5 = min\\(6, 7\\)", prog) |> sum())

    })
  })
})
