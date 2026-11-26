# Changelog

## mighty (development version)

- [`generate_adam_code()`](https://novonordisk-opensource.github.io/mighty/reference/generate_adam_code.md)
  now accepts a connector object from
  [`connector::connect()`](https://novonordisk-opensource.github.io/connector/reference/connect.html)
  as `data_context`. Other input fails early with a clear error
  ([\#48](https://github.com/NovoNordisk-OpenSource/mighty/issues/48)).
- Steps that depend on a metadata (`MD*`) table are no longer always
  commented out in `executable_programs` when a `data_context` is given.
  Available tables are now matched by their name, whatever the connector
  source is called.

## mighty 0.1.0

- Initial CRAN submission.
