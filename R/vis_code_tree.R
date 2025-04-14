vis_code_tree <- function(nodes, edges) {
  edges_ranked <- merge(edges, nodes[, .(node_id, rank)], by = "node_id", all.x = TRUE) |>
    setnames(old = c("node_id", "parent_node"),
             new = c("to", "from")) |>
    setorder(rank)

browser()
  nodes_vs <- copy(nodes)
  nodes_vs <- setnames(nodes_vs, c("node_id", "type"), c("id", "group")) |>
    setorder(rank) |>
    unique(by = "id")

  nodes_vs[, label := toupper(id)]
  nodes_vs[is.na(group), group := "predecessor"]

  # Plot the interactive graph

  RColorBrewer::brewer.pal(8, "Paired")
  domain_colors <-  c("ADLB" = "#FF7F00", "ADSL" = "#1F78B4")
  # Add more colors for other domains if needed
  nodes_vs[, color := domain_colors[as.character(nodes_vs$domain)]]

  visNetwork::visNetwork(nodes_vs, edges_ranked) |>
    visNetwork::visEdges(arrows = "to",
                         smooth = list(type = "cubicBezier", roundness = 0.5)) |>
    visNetwork::visOptions(
      highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ),
      nodesIdSelection = TRUE
    ) |>
    visNetwork::visNodes(shadow = list(enabled = TRUE, size = 10), color = "color") |>
    visNetwork::visGroups(groupname = "column", shape = "dot") |>
    visNetwork::visGroups(groupname = "domain_init", shape = "star", borderWidth=2, size=30) |>
    visNetwork::visGroups(groupname = "row", shape = "box") |>
    visNetwork::visGroups(groupname = "predecessor", shape = "triangle") |>
    visNetwork::visHierarchicalLayout(
      nodeSpacing = 120,
      levelSeparation = 150,
      treeSpacing = 200,
      sortMethod = "directed",
      edgeMinimization = TRUE,
      parentCentralization = TRUE
    ) |>
    visNetwork::visPhysics(
      hierarchicalRepulsion = list(
        nodeDistance = 120,
        centralGravity = 5,
        springLength = 100,
        springConstant = 0.01,
        damping = 0.9
      ),
      solver = "hierarchicalRepulsion"
    )

}
