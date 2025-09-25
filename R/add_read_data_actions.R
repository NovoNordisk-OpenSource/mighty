#' Add Read Data Actions
#'
#' @description
#' Creates and adds read_data actions to the existing actions data table for
#' loading required input data dependencies at the beginning of each program.
#'
#' @param actions Data table containing existing action definitions with columns
#'   including program_id, domain, depend_cols, and outputs.
#'
#' @return
#' Data table combining original actions with new read_data actions, sorted by
#' program_id and rank to ensure proper execution order.
#'
add_read_data_actions <- function(actions) {

  actions_by_pgm <- split(actions, by = "program_id")

  # Extract actions from initial ADaM programs
  min_pgm <- actions |>
    dplyr::group_by(.data$domain) |>
    dplyr::summarise(program_id = min(.data$program_id))
  init_pgm <- min_pgm$program_id
  names(init_pgm) <- min_pgm$domain

  # Extract input dependency columns per program
  read_data_actions <- lapply(actions_by_pgm, function(x) {
    dom <- x$domain[[1]]
    pgm <- x$program_id[[1]]

    if (pgm == init_pgm[[dom]]) {
      dep_cols <- x$depend_cols |>
        rbindlist() |>
        unique() |>
        dplyr::filter(.data$domain != dom)
    } else {
      dep_cols0 <- x$depend_cols |>
        rbindlist() |>
        unique()
      outputs0 <- x$outputs |> unlist()
      dep_cols <- dep_cols0 |>
        dplyr::filter(!(.data$column_name %in% outputs0 & .data$domain == dom))
    }

    # Create read_data actions
    data.table(
      node_id = paste(dom, pgm, "read_data", sep = "-"),
      program_id = pgm,
      rank = 0,
      code_id = "_read_data.mustache",
      type = "read_data",
      depend_cols =  list(data.table(column_name = character(0),
                                     domain = character(0),
                                     domain_type = character(0))),
      outputs = list(paste0(dep_cols$domain, ".", dep_cols$column_name)),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = dom
    )
  }) |> rbindlist()

  # Add read_data actions to existing set of actions
  rbind(actions, read_data_actions) |>
    dplyr::arrange(.data$program_id, .data$rank)

}
