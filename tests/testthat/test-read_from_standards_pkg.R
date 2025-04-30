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

    # Run code to test with tempory library
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

        eval(parse(text = paste0(actual$programs$`1_ADSL`, collapse = "\n")))
        expect_equal(
          unname(ADSL),
          list(
            100,
            "This is a regular string",
            1,
            NULL,
            6,
            5,
            "User-supplied string"
          )
        )

        # Ensure that default arguments are passed as un-evaluated R strings to the program
        prog <- actual$programs$`1_ADSL` |> unlist()
        expect_equal(1, grepl("param_5 = min\\(6, 7\\)", prog) |> sum())

      }
    )
  })
})
