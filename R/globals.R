# Global variable declarations to avoid R CMD CHECK notes
utils::globalVariables(c(
  "has_init",

  # extract_actions variables
  "code_id",
  "domain",
  "parameters_flat",
  "outputs",
  "depend_cols",
  "depend_rows",
  "parameters",
  "id",

  # find_missing_dependencies variables
  "domain_type",
  "column_name",

  # find_remaining_upstream_dependencies variables
  "Var1",
  "Var2",
  "Freq",
  "value",

  # get_adam_dependencies_from_actions variables
  # (domain_type, domain, column_name already declared above)

  # group_actions variables
  "node_id",
  "program_id",
  "type",

  # handle_*_action variables
  # (program_id, code_id, domain already declared above)
  "column_name",

  # initialize_graph_data variables
  # (node_id, domain, Var1, Var2, Freq, value already declared above)

  # merge_ui_with_metadata variables
  "depend_cols_from_code",
  "outputs_from_code",

  # organize_actions variables
  "parent_node",

  # process_column_dependencies variables
  # (domain_type already declared above)

  # process_depend_cols variables
  # (type, depend_cols, domain, outputs already declared above)

  # remove_child_filter_edges variables
  # (type already declared above)

  # render_code variables
  # (domain, program_id already declared above)

  # traverse_and_group_actions variables
  # (node_id, domain, parent_node, type already declared above)

  # update_depend_rows variables
  # (depend_rows, domain already declared above)

  # vis_code_tree variables
  "label",
  "group",
  "color"
))

# Also declare the dot function and setNames if they're causing issues
utils::globalVariables(c(".", "setNames"))
