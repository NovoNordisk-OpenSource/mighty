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
                              path_domain_keys,
                              path_output,
                              data_connection = c("connector", "pharmaverse")) {

  # Data from UI containing explicit user input
  ui_data_1 <- path_ui_data |>
    merge_yaml_files() |>
    parse_ui_data()

  # Enrich derivations in UI data with associated metadata from standard nodes
  ui_data_2 <- path_std_lib |>
    lapply(parse_node_metadata) |>
    unlist(recursive = FALSE) |>
    update_ui_data(ui_data_1)
  domain_initialization_metadata <- ui_data_2$init
  trial_metadata <- ui_data_2$trial_metadata
  nodes <- convert_node_list_to_dt(ui_data_2$nodes)

  # Enrich predecessors in UI data with auto-generated metadata
  nodes_2 <- update_predecessors(nodes, path_domain_keys)
  nodes_3 <- collapse_origin_type_columns(nodes_2)

  nodes_4 <- add_implied_predecessors(nodes_3)
  core_vars <- extract_sdtm_core_variables(nodes_4)
  # extract_sdtm_core_variables_2(nodes_4)
  nodes_5 <- create_domain_initialize_nodes(nodes_4, core_vars, domain_initialization_metadata)
  # This is done before external deps nodes are added (at the moment) because we
  # don't need to explicitly track the external deps in the topology
  edges <- make_edges(nodes_5)
  nodes_6 <- enrich_with_external_dependencies(nodes_5, ui_data_2$init)

  unique_edges <- edges[, .SD[1], by = .(parent_node, node_id)][, .(parent_node, node_id)]
  graph <- igraph::graph_from_data_frame(unique_edges, directed = TRUE, vertices = nodes_5[, .(node_id, domain)])
  topo_order_names <- weighted_node_topo_sort(graph, primary_domain = "adsl")
  vertex_metadata <- nodes_5[, .(node_id, domain, type)]
  program_order <- group_nodes_optimal(topo_order_names, vertex_metadata, edges = edges)

  program_order_2 <- add_program_init_nodes(program_order, nodes_6)
  program_order_3 <- add_nodes_to_load_external_data(program_order_2, nodes_6)

  data_connection <- match.arg(data_connection)
  programs <- generate_program(
    program_order_3,
    nodes_6,
    domain_keys = yaml::read_yaml(path_domain_keys),
    std_library_path = path_std_lib,
    trial_metadata,
    data_connection
  )
  return(list(programs = programs, program_order=program_order, edges=edges, program_order_complete = program_order_3, data_model = ui_data_2$nodes))
}
