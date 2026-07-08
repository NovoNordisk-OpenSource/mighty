# Get started

## Your first mighty ADaM program

Here is a minimal example that demonstrates how to specify and render a
simple ADSL program with mighty to get you acquainted with the core
concepts of the framework.

### Study directory guidelines

The only constraints mighty has on the directory structure is the
presence of a directory that contains **only** the following YAML
specification files:

- Domain specifications (e.g., `adsl.yml`, `adae.yml`, `adlb.yml`)
- `_mighty.yml` — [framework
  configuration](https://novonordisk-opensource.github.io/mighty/articles/mighty_config.qmd)
  (external dataset keys)
- `_study.yml` — study-level properties (optional, used by
  mighty.toolbox for define.xml)

All `.yml`/`.yaml` files in this directory (except `_mighty.yml` and
`_study.yml`) are parsed as domain specifications. Placing
non-specification YAML files (such as a connector config) in the
specifications directory will cause validation errors.

Other files should live outside the specifications directory:

- A [connector config
  file](https://novonordisk-opensource.github.io/connector/) that
  specifies where your trial data is located (SDTM, metadata, and ADaM).
  The `path_connector_config` argument to
  [`generate_adam_code()`](https://novonordisk-opensource.github.io/mighty/reference/generate_adam_code.md)
  is the file path to the connector config file, embedded verbatim into
  the generated programs.
- Study-specific custom components
- A location where the compiled ADaM programs can be written out to

### Set up your framework configuration file

The `_mighty.yml` file provides framework configuration that spans
across all ADaM domains. This includes primary keys for external
datasets that are used for joining tables.

For this example, we will focus on a simple ADSL table and only provide
keys related to that domain.

**\_mighty.yml**

### Set up your ADaM specification

Provide a YAML file that specifies which columns to include in ADSL and
how they are derived. The specification **only contains columns that
belong in the final dataset**; source columns needed only during
derivation are handled through component dependencies instead. See
[`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md)
for details.

This ADSL table contains 6 columns: 4 simple columns from the base
domain, 1 renamed column, and 1 derivation using a custom component.

**adsl.yml**

### Define your custom component

The column `AGE_GRP1` in ADSL is derived by using a custom component
that contains the logic to process the ADSL table by deriving the
additional column `AGE_GRP1`.

**age_group_01.R**

### Render the ADaM program

Call mighty to consolidate, analyze and render your ADSL program:

**example_01_main.R**

Printing the rendered ADaM program yields a complete program for
generating ADSL based on the specifications
