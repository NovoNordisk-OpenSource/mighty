test_that("multiplication works", {
  # SETUP ----------------------------------------------------------------------
  ui_path <- c(
    test_path("fixtures", "adsl_0001_new.yml"),
    test_path("fixtures", "adlb_0001_new.yaml")
  )
  init_yml_path <- test_path("fixtures", "init_0001.yml")
  std_lib_path <- c(
    testthat::test_path("fixtures", "adsl_0001.R"),
    testthat::test_path("fixtures", "adlb_0001.R")
  )

  ui_data <- read_adam_specs(ui_path) |>
  convert_yml_to_data_table()
  trial_metadata <- yaml::read_yaml(init_yml_path)
  nodes <- std_lib_path |>
    lapply(parse_node_metadata) |>
    unlist(recursive = FALSE) |>
    update_ui_data(ui_data) |>
    add_node_id()
browser()
})
