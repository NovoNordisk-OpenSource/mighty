# Offline example study

A small study that `generate_adam_code()` can process without network access.
It is used by the function's example and by `tests/testthat/test-generate_adam_code.R`.

- `study_dir/` holds an ADSL and an ADAE specification. Together they use every standard action type.
- `components/` holds a copy of the standard components from
  <https://github.com/NovoNordisk-OpenSource/mighty.standards> (folder `components`,
  commit `b3eb6a3833736f015c2fd919ca9f51f22b5cc323`), plus the custom component `age_group_01.R`.
