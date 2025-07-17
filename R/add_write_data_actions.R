add_write_data_actions <- function(actions) {

  # Split actions by program
  actions_split <- split(actions[,c("domain", "program_id", "rank", "type", "outputs")], by = "program_id")

  # Create a write_data for each program
  write_actions <- lapply(actions_split, function(x) {

    dom <- x$domain[[1]]
    pgm <- x$program_id[[1]]

    # Specify column dependencies as any outputs from actions apart from the
    # read_data action which only returns internal columns
    depend_cols <- data.table(
      column_name = x[x$type != "read_data", ]$outputs |> unlist() |> unique(),
      domain = dom
    )
    depend_cols[["domain_type"]] <- classify_data_domains(dom)

    # Create write_data action
    data.table(
      node_id = paste(dom, pgm, "write_data", sep = "-"),
      program_id = pgm,
      rank = max(x$rank) + 1,
      code_id = "_write_data",
      type = "write_data",
      depend_cols = list(depend_cols),
      outputs = list(NA),
      depend_rows = list(NA),
      parameters = list(NA),
      domain = dom
    )
  }) |> rbindlist()

  # Update set of actions with write_data actions
  actions_updated <- rbind(actions, write_actions) |>
    dplyr::arrange(program_id, rank)

  return(actions_updated)
}
