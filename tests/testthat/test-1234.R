test_that("perfomance", {
  # SETUP
  ui_path <- c(
    test_path("fixtures", "adsl_1234.yml"),
    test_path("fixtures", "adlb_1234.yml")
  )
  path_trial_metadata <- test_path("fixtures", "init_0001.yml")
  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )

  domain_keys_path <- system.file("standards", "domain_keys.yml", package = "mighty")
  output_path <- withr::local_tempdir()

  # ACT

  actual <- generate_adam_code(ui_path,
                               std_lib_path,
                               path_trial_metadata,
                               domain_keys_path,
                               output_path,
                               data_connection = "pharmaverse")
  write_adam_programs(dir = output_path, programs = actual$programs)
  browser()
  x <- list.files(output_path, full.names = TRUE)
  do.call(file.edit, as.list(x))
  programs <- lapply(x, readLines)
  names(programs) <- basename(x)

})
