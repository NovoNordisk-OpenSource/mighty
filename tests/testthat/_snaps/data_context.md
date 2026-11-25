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

# as_data_context() asks to pick one config from nested_connectors

    Code
      as_data_context(cnt)
    Condition
      Error:
      ! `data_context` must be a single <connectors> object.
      x You supplied a <nested_connectors> object with configs "study_a" and "study_b".
      i Pick one config, e.g. `data_context = cnt[["study_a"]]`.

