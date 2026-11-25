# as_data_context() rejects input that is not a connectors object

    Code
      as_data_context(list())
    Condition
      Error:
      ! `data_context` must be a <connectors> object or `NULL`.
      x You supplied an empty list.
      i Create one with `connector::connect("_connector.yml")`.

---

    Code
      as_data_context("_connector.yml")
    Condition
      Error:
      ! `data_context` must be a <connectors> object or `NULL`.
      x You supplied a string.
      i Create one with `connector::connect("_connector.yml")`.

