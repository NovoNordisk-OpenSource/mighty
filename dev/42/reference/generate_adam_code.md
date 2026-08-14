# Generate Complete Set of ADaM Programs

This function orchestrates the complete workflow for generating ADaM
(Analysis Data Model) programs from ADaM specifications and trial
metadata. It processes specifications, validates dependencies, creates
action sequences, and renders executable R code for ADaM dataset
creation.

## Usage

``` r
generate_adam_code(
  adam_specifications,
  path_connector_config,
  check_cross_domain_adam_dependencies = TRUE,
  data_context = NULL
)
```

## Arguments

- adam_specifications:

  Character string. Directory path containing ADaM specification YAML
  files (one per domain) and optionally a `_mighty.yml` framework
  configuration file with external dataset definitions and keys.

- path_connector_config:

  Character string. File path to the connector configuration file (e.g.,
  `"_connector.yml"`). This path is inserted exactly as written into the
  generated programs (no validation or transformation is performed). The
  generated programs use this path to connect to data when they
  execute - mighty itself does not read or validate the connector
  configuration file. Prefix with `!expr ` to embed an R expression that
  is evaluated at runtime by the generated program (e.g.,
  `'!expr here::here("_connector.yml")'`).

- check_cross_domain_adam_dependencies:

  Logical. If `TRUE` (default), validates dependencies across different
  ADaM domains. If `FALSE`, only validates dependencies within
  individual domains.

- data_context:

  Optional list or environment providing additional context about
  available data sources for executable program generation. If `NULL`,
  all programs are considered potentially executable.

## Value

A named list containing the complete ADaM program generation results:

- programs:

  Named list of complete R programs, one per ADaM domain. Each element
  is a character string containing a fully executable R script. Programs
  include all dependency-ordered actions and are ready to execute
  independently. Names are prefixed with the required execution order.

- program_sequence:

  *For debugging only*. Data.table containing the complete action
  sequence with fully rendered R code for all programs. Each row
  represents a single action with action metadata. Actions are ordered
  by dependency requirements within each domain. This provides a
  detailed view of the execution plan before compilation into complete
  programs.

- executable_programs:

  Named list of programs that can be executed with the currently
  available data sources (as determined by `data_context`). Structure
  identical to `programs` but includes only code where all required
  input data is available. If `data_context` is NULL, this will match
  `programs`. Used to identify which ADaM derivations can be generated
  given the current data availability.

- executable_program_sequence:

  *For debugging only*. Data.table containing the action sequence for
  executable programs only. Structure similar to `program_sequence` but
  filtered to include only actions from code that can be executed with
  available data. Provides visibility into which specific
  transformations will run when executing the available programs.

- edges:

  *For debugging only*. Data.table defining the dependency graph between
  actions. Contains columns `parent_node` and `node_id`, representing
  directed edges where parent actions must execute before child actions.
  Edges are created from both column dependencies (when one action
  produces a column another action consumes) and row dependencies
  (explicit row-level operations). Includes synthetic edges connecting
  actions with no dependencies to domain initialization actions.
  Self-referential edges are removed. Used for debugging action
  execution order and dependency resolution.

- actions:

  *For debugging only*. Data.table containing base action configurations
  before code rendering and program organization. Each row represents a
  single action with columns: `node_id` (unique action identifier),
  `domain` (ADaM domain name), `code_id` (reference to code component or
  NA), `type` (action type: col_copy, col_rename, col_mutate, col_echo,
  col_compute, row_compute, init_domain, or filter_domain), `outputs`
  (list column of character vectors showing columns produced),
  `depend_cols` (nested data.table with column_name, domain, and
  domain_type showing column dependencies), `depend_rows` (list of
  node_ids this action depends on for row operations), and `parameters`
  (named list of user-provided parameters). Reflects the internal action
  data model before execution ordering. Used for debugging action setup
  and dependency validation.

- rendered_components:

  *For debugging only*. Named list of code components that were
  successfully rendered using Mustache templates during the generation
  process. Each element contains the rendered R code for a specific
  component (e.g., derivation functions). Component names correspond to
  `code_id` values referenced in the action specifications. Used for
  inspecting how template parameters were resolved and for debugging
  component rendering.

## Details

The function executes the following workflow:

1.  Reads and validates ADaM specifications and trial metadata

2.  Sets up initial action configurations with dependency validation

3.  Adds domain initialization, filtering, and data reading actions

4.  Creates dependency graph and organizes actions in execution order

5.  Adds data writing actions and checks executable status

6.  Renders R code for both complete and executable program sets

7.  Compiles individual actions into complete, runnable programs

Each generated program includes all necessary data transformations,
derivations, and output operations for creating a specific ADaM dataset
according to the provided specifications.

## File Requirements

- ADaM specification file must be a valid YAML file with ADaM
  specifications following the schema defined in mighty.metadata

- Trial metadata file must contain valid study configuration

- Connector configuration file path must be valid for the generated
  programs to execute

## Error Handling

The function will stop execution if:

- ADaM specifications or trial metadata files cannot be read or are
  invalid

- Dependency validation fails (missing required columns)

- Trial configuration is malformed

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate ADaM programs with full dependency checking
result <- generate_adam_code(
  adam_specifications = "path/to/yaml_specs_directory",
  path_connector_config = "path/to/trial_directory/_connector.yml",
  check_cross_domain_adam_dependencies = TRUE
)

# Access generated programs
adsl_program <- result$programs$ADSL
executable_programs <- result$executable_programs
} # }
```
