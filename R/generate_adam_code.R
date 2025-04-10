#' Generates the complete set of ADaM programs
#'
#' @param path_ui_data
#' @param path_std_lib
#' @param path_domain_keys
#' @param path_output
#' @param data_connection
#'
#' @return
#' @export
#'
#' @examples
generate_adam_code <- function(path_ui_data,
                              path_std_lib,
                              path_trial_metadata,
                              path_domain_keys,
                              path_output,
                              data_connection = c("connector", "pharmaverse"),
                              trial_metadata_path) {

 # Data from UI containing explicit user input

  ui_yml <- read_adam_specs(path_ui_data)
 ui_table <- convert_yml_to_data_table(ui_yml)
 trial_metadata <- yaml::read_yaml(path_trial_metadata)
 nodes_1 <- path_std_lib |>
    lapply(parse_node_metadata) |>
    unlist(recursive = FALSE) |>
    update_ui_data(ui_table) |>
    add_node_id()


browser()
# Enrich predecessors in UI data with auto-generated metadata
nodes_2 <- update_predecessors(nodes_1, path_domain_keys)

# Enrich UI data with required predecessor actions not in the UI data and that
# are required for the derivations to be run
nodes_3 <- add_implied_predecessors(nodes_2)

# Create an initialization action per domain that consumes predecessor actions
nodes_4 <- create_domain_initialize_nodes(nodes_3, ui_yml$init)

# Add information about external dependencies to the initialization action
nodes_5 <- enrich_with_external_dependencies(nodes_4, ui_yml$init)

# Identify edges in the topology graph
edges <- make_edges(nodes_5)

# Identify topological order of actions
nodes_topo_order <- weighted_node_topo_sort(edges, nodes_5, primary_domain = "adsl")

# Group actions into programs that can be run as batches in a sequence
program_sequence_1 <- group_nodes_optimal(nodes_topo_order, nodes_5, edges)

# Add initialization actions to the program sequence
program_sequence_2 <- add_program_init_nodes(program_sequence_1, nodes_5)

# Add action to import external dependencies to the program sequence
program_sequence_3 <- add_nodes_to_load_external_data(program_sequence_2, nodes_5)

# Create programs
data_connection <- match.arg(data_connection)
programs <- generate_program(
  program_sequence_3,
  nodes_5,
  yaml::read_yaml(path_domain_keys),
  path_std_lib,
  ui_yml,
  data_connection
)

return(list(program_sequence = program_sequence_3,
  edges = edges,
  data_model = ui_yml$nodes))
}
