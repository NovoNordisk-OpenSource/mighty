# Invalid data_context fails before component repos are fetched

    Code
      generate_adam_code(adam_specifications = ui_data, data_context = list())
    Condition
      Error in `generate_adam_code()`:
      ! `data_context` must be a <connectors> object or `NULL`.
      x You supplied an empty list.
      i Create one with `connector::connect("_connector.yml")`.

