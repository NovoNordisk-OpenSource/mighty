test_that("multiplication works", {
  # SETUP ----------------------------------------------------------------------
  ui_path <- c(
    test_path("fixtures", "init_0001.yml"),
    test_path("fixtures", "adsl_0001_new.yml"),
    test_path("fixtures", "adlb_0001_new.yaml")
  )
  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )

  ui_data_1 <- read_adam_domain_yml(ui_path[2])
  ui_data_2 <- std_lib_path |>
    lapply(parse_node_metadata) |>
    unlist(recursive = FALSE) |>
    update_ui_data_2(ui_data_1)
})
