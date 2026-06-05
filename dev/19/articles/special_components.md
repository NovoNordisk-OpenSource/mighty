# Special components

Special components are Mustache templates (prefixed with `mighty_`) that
mighty inserts automatically into every generated ADaM program. They
live in mighty.standards and are resolved via the `repos` mechanism.
Users never reference them in YAML specs. This vignette documents the
parameter contract for each special component so that template authors
can understand (and modify) the templates.

## Three stages of code generation

Mighty produces ADaM programs through three distinct stages, each with
its own environment. Knowing which stage you’re looking at tells you
what values are available and what the parameters mean.

1.  **Mighty pipeline** — mighty reads YAML specs, resolves
    dependencies, and decides which components to use and in what order.
    This is where the parameter lists described below get built. The
    environment is mighty’s own R session with full access to mighty
    internals.

2.  **Template rendering** — mighty.component renders each Mustache
    template by substituting parameters into `{placeholders}`. The
    template sees only the named list that mighty passes to it.

3.  **ADaM program execution** — the rendered R code runs later, in the
    user’s own R session, as a standalone script. It has access to the
    `connector` package, `dplyr`, and whatever the user’s project loads
    — but not to mighty or mighty.component.

This vignette documents the named lists that mighty passes at the
Template rendering stage.

## Template syntax

The parameter examples in later sections show values that get
substituted into Mustache placeholders. Here’s how interpolation works
in these templates.

Mustache has two interpolation forms:

| Syntax | Behavior |
|----|----|
| `{{variable}}` | Inserts the value with HTML escaping — `"` becomes `&quot;`, `<` becomes `&lt;`, etc. |
| `{{{variable}}}` | Inserts the value verbatim — no escaping |

All variable interpolations in these templates use triple-mustache
`{{{variable}}}`, in both `@code` sections and documentation headers
(`@title`, `@description`, `@param`, `@depends`, `@outputs`). This
avoids HTML escaping in generated code and defers any format-specific
rendering to mighty.component.

The only exception is **section tags** (`{{#condition}}`,
`{{/condition}}`, `{{^condition}}`), which are always double-mustache —
this is a Mustache language requirement.

## The `keep_vars` pipeline

Before diving into each template, it helps to understand one parameter
that appears in four of them with different contents each time.

`keep_vars` appears in the four structural templates
(`mighty_read_data`, `mighty_init_domain`, `mighty_filter_domain`,
`mighty_write_data`) but not in the column-level templates
(`mighty_col_mutate`, `mighty_col_rename`, `mighty_col_echo`). It
carries a different set of columns at each stage:

| Template | `keep_vars` contains | Purpose |
|----|----|----|
| `mighty_read_data` | All columns needed by any downstream step, per source domain | Limits memory footprint on read |
| `mighty_init_domain` | Same list as `mighty_read_data` | Selects columns after row binding |
| `mighty_filter_domain` | Columns surviving after filtering, minus `SRC_` and filter-only columns | Drops columns no longer needed post-filter |
| `mighty_write_data` | Final output columns in YAML spec order | Controls what is persisted and in what order |

`keep_vars` in `mighty_read_data` is the origin: every column referenced
in later templates must have been loaded there first. If a column
appears in `mighty_init_domain`, `mighty_filter_domain`, or
`mighty_write_data` but was not included in `mighty_read_data`, the
generated program will fail at execution time with a column not found
error. Mighty computes all four lists from the same YAML specification
so this is guaranteed automatically — but it matters when modifying
templates or building custom pipelines.

## mighty_read_data

Rendered at the top of every ADaM program. Opens a connector and reads
each source dataset, selecting only the columns needed downstream.

### Example

Given this YAML specification:

``` yaml
id: ADSL
keys:
  - USUBJID
  - STUDYID
population:
  base:
    - domain: DM
      depends: [NA]
      filter: NA
    - domain: DM_VACCINE
      depends: [NA]
      filter: NA
columns:
  - id: USUBJID
  - id: STUDYID
  - id: ARM
```

Mighty builds the following parameter list and passes it to the
template:

``` r

list(
  connector_path_expr = '"_connector.yml"',
  domains = list(
    list(
      is_current_domain = FALSE,
      domain_name = "DM",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    ),
    list(
      is_current_domain = FALSE,
      domain_name = "DM_VACCINE",
      data_type = "sdtm",
      keep_vars = "ARM, STUDYID, USUBJID"
    )
  )
)
```

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Downloading repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Successfully downloaded and cached "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_read_data" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_read_data" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Read source data
    #' @description Opens a connector and reads each source dataset into memory,
    #'   retaining only the columns needed downstream. Rendered once at the top of
    #'   every generated ADaM program.
    #' @type internal
    #' @param connector_path_expr Character string. R expression that evaluates to
    #'   the connector configuration file path. Either a quoted literal path or a
    #'   bare R expression evaluated at runtime (e.g. `here::here("_connector.yml")`).
    #' @param domains List of lists. Each element represents a domain/dataset to be read, containing the following components:
    #'   \describe{
    #'     \item{is_current_domain}{Logical. `TRUE` to read the entire dataset (e.g. an ADSL update program reading its own earlier output); `FALSE` to apply column selection.}
    #'     \item{domain_name}{Character string. Name of the domain to read, uppercase (e.g. `"DM"`, `"ADSL"`).}
    #'     \item{data_type}{Character string. Connector data source to use: `"sdtm"`, `"adam"`, or `"metadata"`.}
    #'     \item{keep_vars}{Character string. Comma-separated column names to select. Ignored when `is_current_domain` is `TRUE`.}
    #'   }
    #'
    #'   Example:
    #'   \preformatted{
    #'   domains <- list(
    #'     list(
    #'       is_current_domain = TRUE,
    #'       domain_name = "ADSL",
    #'       data_type = "adam",
    #'       keep_vars = "ARM_MATCH, COUNTRY, NEWFL01, USUBJID"
    #'     ),
    #'     list(
    #'       is_current_domain = FALSE,
    #'       domain_name = "ADLB",
    #'       data_type = "adam",
    #'       keep_vars = "AVAL, LBTEST, USUBJID"
    #'     )
    #'   )
    #'   }
    #' @code
    cnt <- connector::connect(config = {{{connector_path_expr}}})
    {{#domains}}
      {{#is_current_domain}}
      {{{domain_name}}} <- cnt${{{data_type}}}$read_cnt(tolower('{{{domain_name}}}'))
      {{/is_current_domain}}
      {{^is_current_domain}}
      {{{domain_name}}} <- cnt${{{data_type}}}$read_cnt(tolower('{{{domain_name}}}')) |>
      dplyr::select({{{keep_vars}}})
      {{/is_current_domain}}
      {{/domains}}

**Rendered output**

``` r
cnt <- connector::connect(config = "_connector.yml")
  DM <- cnt$sdtm$read_cnt(tolower('DM')) |>
  dplyr::select(ARM, STUDYID, USUBJID)
  DM_VACCINE <- cnt$sdtm$read_cnt(tolower('DM_VACCINE')) |>
  dplyr::select(ARM, STUDYID, USUBJID)
```

### Parameters

#### `connector_path_expr`

A string containing an R expression that resolves to the connector
config file path at runtime. Two forms are possible:

| Config value | `connector_path_expr` |
|----|----|
| `_connector.yml` | `'"_connector.yml"'` (quoted literal) |
| `!expr here::here("_connector.yml")` | `'here::here("_connector.yml")'` (bare expression) |

The `!expr` prefix in the mighty config strips the prefix and passes the
remaining expression through verbatim. Plain paths are wrapped in double
quotes so they evaluate as character literals in the generated R code.

#### `domains`

A list where each element represents one source dataset to read. Each
element is a named list with the following fields:

| Field | Type | Meaning |
|----|----|----|
| `domain_name` | character | Source dataset name, uppercase (e.g. `"DM"`, `"ADSL"`). |
| `data_type` | character | Which connector data source to use: `"sdtm"`, `"adam"`, or `"metadata"`. |
| `keep_vars` | character | Comma-separated column names to select, sorted alphabetically and uppercased. Only used when `is_current_domain` is `FALSE`. Column order in the final dataset is controlled by `keep_vars` in `mighty_write_data`, not by this field. |
| `is_current_domain` | logical | `TRUE` when the domain being read is the same one being built. When `TRUE`, `keep_vars` should be ignored. |

##### How `data_type` is determined

Domain names are classified using these rules:

| Pattern | `data_type` |
|----|----|
| Two-character names (`DM`, `LB`, `VS`, …), `RELREC`, `DM_*`, `SUPP*` | `"sdtm"` |
| Names starting with `AD` (`ADSL`, `ADLB`, …) | `"adam"` |
| Names starting with `MD` | `"metadata"` |

##### When `is_current_domain` is `TRUE`

This happens in multi-program domains where a later program reads the
output of an earlier program for the same domain (e.g. an ADSL “update”
program). When `is_current_domain` is `TRUE`, `keep_vars` should be
ignored.

## mighty_init_domain

Rendered once per program, after `mighty_read_data`. Combines one or
more source datasets by row binding and selects the columns needed for
the domain.

### Example

Using the same ADSL specification from `mighty_read_data`:

``` r

list(
  self = "ADSL",
  keep_vars = "ARM, STUDYID, USUBJID",
  source_domain_rbind = "rbind(DM,\nDM_VACCINE)",
  src_mutations = list()
)
```

`src_mutations` is empty here because both source domains have
`filter: NA`. When domain-specific filters are defined, mighty populates
it so that a temporary `SRC_` column can be added before row binding
(see [ADaM specification — filter execution
order](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.html#filter-execution-order)):

``` r

src_mutations = list(
  list(domain = "DM"),
  list(domain = "DM_VACCINE")
)
```

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_init_domain" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_init_domain" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Initialize ADaM domain
    #' @description Combines one or more source datasets by row binding, optionally
    #'   tagging each row with a `SRC_` tracking column, and selects the columns
    #'   needed for the domain. Rendered once per program, after `_read_data`.
    #' @type internal
    #' @param self Character string. Name of the ADaM domain being initialised (e.g. `"ADSL"`, `"ADLB"`).
    #' @param keep_vars Character string. Comma-separated column names to select after row binding.
    #' @param source_domain_rbind Character string. Pre-formatted R expression that combines source datasets. A single domain is passed as its name; multiple domains are wrapped in `rbind()`.
    #' @param src_mutations List of lists. Per-domain mutation specifications used to add a `SRC_` tracking column before row binding. Each element has a single field `domain` (e.g. `list(list(domain = "LB"), list(domain = "XL"))`). Empty list when `SRC_` is not needed.
    #' @code
    {{#src_mutations}}
    {{{domain}}} <- {{{domain}}} |>
      dplyr::mutate(SRC_ = "{{{domain}}}")

    {{/src_mutations}}
    {{{self}}} <- {{{source_domain_rbind}}} |>
        dplyr::select({{{keep_vars}}}) |>
        admiral::convert_blanks_to_na()

**Rendered output**

``` r
ADSL <- rbind(DM,
DM_VACCINE) |>
    dplyr::select(ARM, STUDYID, USUBJID) |>
    admiral::convert_blanks_to_na()
```

### Parameters

#### `self`

The name of the ADaM domain being built (e.g. `"ADSL"`, `"ADLB"`). Used
as the variable name on the left-hand side of the assignment.

#### `keep_vars`

Comma-separated column names to retain after row binding. Pre-formatted
in R before being passed to the template.

#### `source_domain_rbind`

A pre-formatted R expression that combines source datasets. A single
source domain is passed as its name directly; multiple domains are
wrapped in an [`rbind()`](https://rdrr.io/r/base/cbind.html) call with
line breaks for readability.

#### `src_mutations`

A list of per-domain mutation specifications used to add a `SRC_`
tracking column before row binding. Each element has a single field:

| Field    | Type      | Meaning                                       |
|----------|-----------|-----------------------------------------------|
| `domain` | character | Name of the source domain to tag with `SRC_`. |

`src_mutations` is empty when `SRC_` is not in `keep_vars`.

`SRC_` is an internal column that mighty adds and removes automatically
to support domain-specific row filters. See the [ADaM specification
vignette](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.html#filter-execution-order)
for a full explanation of how filtering works across source domains.

## mighty_filter_domain

Rendered after `mighty_init_domain`. Applies joins, domain-specific
filters, global filters, and column selection to the initialized
dataset.

### Example

Given this YAML specification:

``` yaml
id: ADLB
keys:
  - USUBJID
  - STUDYID
population:
  base:
    - domain: LB
      depends: [NA]
      filter: NA
    - domain: XL
      depends: [LBCAT]
      filter: "LBCAT == 'CHEMISTRY'"
  global:
    - filter: "!is.na(SEX)"
      depends: [ADSL.SEX]
columns:
  - id: USUBJID
  - id: STUDYID
  - id: PARAMCD
  - id: LBCAT
  - id: SEX
    method: ADSL.SEX
```

Mighty builds the following parameter list and passes it to the
template:

``` r

list(
  self = "ADLB",
  joins = list(
    list(
      table = "ADSL",
      select_expr = "SEX, STUDYID, USUBJID",
      keys = '"STUDYID", "USUBJID"'
    )
  ),
  domain_filter = "(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')",
  global_filter = "!is.na(SEX)",
  keep_vars = "LBCAT, PARAMCD, STUDYID, USUBJID"
)
```

> **Note**
>
> `SEX` appears in the global filter (`!is.na(SEX)`) and is declared in
> the YAML spec, but it is absent from `keep_vars`. This is intentional:
> `SEX` originates from `ADSL` (via `method: ADSL.SEX`) and is not
> present in the LB or XL source data. It is joined in temporarily to
> evaluate the filter, then dropped. A subsequent `mighty_col_echo` step
> re-adds `SEX` to ADLB permanently via a left join with ADSL.

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_filter_domain" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_filter_domain" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Filter ADaM domain
    #' @description Applies joins needed for filter evaluation, domain-specific row
    #'   filters (via the internal `SRC_` column), global row filters, and final
    #'   column selection. Rendered after `_init_domain`.
    #' @type internal
    #' @param self Character string. Name of the ADaM domain being filtered (e.g. `"ADLB"`).
    #' @param joins List of join specifications used to bring in columns required for
    #'   filter evaluation. Each element is a list containing:
    #'   \describe{
    #'     \item{table}{Character string. Name of the dataset to join with.}
    #'     \item{select_expr}{Character string. Comma-separated column names to select from the join dataset.}
    #'     \item{keys}{Character string. Comma-separated quoted column names used as join keys (e.g. `"\"STUDYID\", \"USUBJID\""`).}
    #'   }
    #'   Empty list when no filter depends on an external dataset.
    #' @param domain_filter Character string or `NULL`. Pre-formatted R expression filtering on `SRC_` (e.g. `"(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')"`). `NULL` when no domain-specific filter is defined — the filter block is skipped.
    #' @param global_filter Character string or `NULL`. Raw filter expression from the YAML spec (e.g. `"!is.na(SEX)"`). `NULL` when no global filter is defined — the filter block is skipped.
    #' @param keep_vars Character string or `NULL`. Comma-separated column names to retain. `NULL` when no selection is needed — the select block is skipped. `SRC_` is excluded automatically.
    #' @code
    {{#joins}}
    {{{self}}} <- {{{self}}} |>
      dplyr::left_join({{{table}}} |> dplyr::select({{{select_expr}}}),
                       by = c({{{keys}}}))

    {{/joins}}
    {{#domain_filter}}
    {{{self}}} <- {{{self}}} |>
      dplyr::filter({{{domain_filter}}}) |>
      dplyr::select(-SRC_)

    {{/domain_filter}}
    {{#global_filter}}
    {{{self}}} <- {{{self}}} |>
      dplyr::filter({{{global_filter}}})

    {{/global_filter}}
    {{#keep_vars}}
    {{{self}}} <- {{{self}}} |>
      dplyr::select({{{keep_vars}}})

    {{/keep_vars}}

**Rendered output**

``` r
ADLB <- ADLB |>
  dplyr::left_join(ADSL |> dplyr::select(SEX, STUDYID, USUBJID),
                   by = c("STUDYID", "USUBJID"))

ADLB <- ADLB |>
  dplyr::filter((SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')) |>
  dplyr::select(-SRC_)

ADLB <- ADLB |>
  dplyr::filter(!is.na(SEX))

ADLB <- ADLB |>
  dplyr::select(LBCAT, PARAMCD, STUDYID, USUBJID)
```

### Parameters

#### `self`

The name of the ADaM domain being filtered.

#### `joins`

A list of join specifications. Each element is a named list:

| Field | Type | Meaning |
|----|----|----|
| `table` | character | Name of the dataset to join with. |
| `select_expr` | character | Comma-separated column names to select from the join dataset. |
| `keys` | character | Comma-separated quoted column names used as join keys. |

`joins` is empty when no filter depends on an external dataset.

#### `domain_filter`

Pre-formatted R expression filtering on `SRC_` (e.g.
`"(SRC_ == 'LB') | (SRC_ == 'XL' & LBCAT == 'CHEMISTRY')"`). `NULL` when
no domain-specific filter is defined — whisker treats `NULL` as falsy so
the filter block is suppressed.

#### `global_filter`

Raw filter expression string from the YAML spec (e.g. `"!is.na(SEX)"`).
`NULL` when no global filter is defined — the filter block is
suppressed.

#### `keep_vars`

A comma-separated string of column names to retain. `NULL` when no
selection is needed — whisker treats `NULL` as falsy so the
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
call is suppressed. `SRC_` is excluded because it is only used during
filtering.

## mighty_col_mutate

Rendered for each `col_copy` action — variables copied verbatim from one
column to another, typically with an ADaM-standard rename
(e.g. `LBSTRESN` → `AVAL`).

### Example

Consider the following YAML specification snippet. `AVAL` is defined
with `method: LBSTRESN`, and `LBSTRESN` is also listed as its own
column. Mighty copies the values rather than renaming because the source
column is retained in the output:

``` yaml
columns:
  - id: LBSTRESN
  - id: AVAL
    method: LBSTRESN
```

Mighty builds the following parameter list and passes it to the
template:

``` r

list(
  self = "ADLB",
  rename_var = "AVAL",
  source_var = "LBSTRESN"
)
```

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_mutate" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_mutate" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Copy column
    #' @description Copies values from an existing column into a new column using
    #'   `dplyr::mutate()`. Used when the source column is also retained in the
    #'   output; see `_col_rename` when the source column should be replaced.
    #' @type internal
    #' @param self Character string. Name of the dataset being modified (e.g. `"ADLB"`).
    #' @param rename_var Character string. Name of the new column to create (e.g. `"AVAL"`).
    #' @param source_var Character string. Name of the existing column to copy from (e.g. `"LBSTRESN"`).
    #' @code
    {{{self}}} <- {{{self}}} |> dplyr::mutate({{{rename_var}}} = {{{source_var}}})

**Rendered output**

``` r
ADLB <- ADLB |> dplyr::mutate(AVAL = LBSTRESN)
```

### Parameters

#### `self`

The name of the dataset being modified.

#### `rename_var`

The name of the new column to create.

#### `source_var`

The name of the existing column whose values are copied.

## mighty_col_rename

Rendered for each `col_rename` action — renames an existing column
in-place without copying values. Unlike `mighty_col_mutate`, which
creates a new column via
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
this template uses
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
and leaves no extra column behind.

Both templates share the same parameter list and are served by the same
internal function (`params_mutate_code`). They differ only in the R verb
the template emits.

### Example

Consider the following YAML specification snippet. `SRCSEQ` is defined
with `method: LBSEQ`, and `LBSEQ` is **not** listed as its own column.
Because the source column has no independent entry, mighty renames it
rather than copying it:

``` yaml
columns:
  - id: SRCSEQ
    method: LBSEQ
```

Mighty builds the following parameter list and passes it to the
template:

``` r

list(
  self = "ADLB",
  rename_var = "SRCSEQ",
  source_var = "LBSEQ"
)
```

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_rename" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_rename" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Rename column
    #' @description Renames an existing column in-place using `dplyr::rename()`,
    #'   leaving no extra column behind. Used when the source column has no
    #'   independent entry in the YAML spec; see `_col_mutate` when the source
    #'   column is also retained in the output.
    #' @type internal
    #' @param self Character string. Name of the dataset being modified (e.g. `"ADLB"`).
    #' @param rename_var Character string. New column name after renaming (e.g. `"SRCSEQ"`).
    #' @param source_var Character string. Existing column to rename (e.g. `"LBSEQ"`).
    #' @code
    {{{self}}} <- {{{self}}} |> dplyr::rename({{{rename_var}}} = {{{source_var}}})

**Rendered output**

``` r
ADLB <- ADLB |> dplyr::rename(SRCSEQ = LBSEQ)
```

### Parameters

#### `self`

The name of the dataset being modified.

#### `rename_var`

The new column name after renaming.

#### `source_var`

The existing column to rename.

## mighty_col_echo

Rendered for each `col_echo` action — variables pulled in from a
different domain via a left join.

### Example

Consider the following YAML specification snippet. The dot notation
`ADSL.SEX` in `method:` tells mighty the column originates from another
domain, which triggers a left join rather than a direct copy:

``` yaml
columns:
  - id: SEX
    method: ADSL.SEX
```

Mighty builds the following parameter list and passes it to the
template:

``` r

list(
  self = "ADLB",
  join_dataset = "ADSL",
  select_expr = "STUDYID, USUBJID, SEX",
  by_vars = '"STUDYID", "USUBJID"',
  needs_rename = FALSE,
  output_var = "SEX",
  var_to_add = "SEX"
)
```

This is the step that permanently adds `SEX` to ADLB — the same column
that `mighty_filter_domain` joined in temporarily to evaluate the global
filter.

The standard template and a rendered version using the parameter list
above can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_echo" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_col_echo" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Echo column from another domain
    #' @description Brings a column from another domain into the current dataset
    #'   via a `dplyr::left_join()`. Used when `method:` in the YAML specification
    #'   uses dot notation (e.g. `ADSL.SEX`) to reference a cross-domain variable.
    #' @type internal
    #' @param self Character string. Name of the primary dataset being modified; the left-hand side of the join, whose row count is preserved (e.g. `"ADLB"`).
    #' @param join_dataset Character string. Name of the dataset to look up from; the right-hand side of the join (e.g. `"ADSL"`).
    #' @param select_expr Character string. Comma-separated column names to select from `join_dataset` before joining. Includes both join keys and the variable being added.
    #' @param by_vars Character string. Comma-separated quoted column names used as join keys, ready for `dplyr::left_join(by = c(...))` (e.g. `"\"STUDYID\", \"USUBJID\""`).
    #' @param needs_rename Logical. `TRUE` when the variable name in `join_dataset` differs from the desired output name, triggering an appended `dplyr::rename()` call.
    #' @param output_var Character string. Desired column name in `self` after the join.
    #' @param var_to_add Character string. Column name as it exists in `join_dataset`, before any renaming. Equal to `output_var` when `needs_rename` is `FALSE`.
    #' @code
    {{{self}}} <- {{{self}}} |>
        dplyr::left_join({{{join_dataset}}} |> dplyr::select({{{select_expr}}}),
    by = c({{{by_vars}}})){{#needs_rename}} |>
                         dplyr::rename({{{output_var}}} = {{{var_to_add}}}){{/needs_rename}}

**Rendered output**

``` r
ADLB <- ADLB |>
    dplyr::left_join(ADSL |> dplyr::select(STUDYID, USUBJID, SEX),
by = c("STUDYID", "USUBJID"))
```

### Parameters

#### `self`

The name of the primary dataset being modified.

#### `join_dataset`

The name of the dataset to join with (right-hand side of the join).

#### `select_expr`

Comma-separated column names to select from `join_dataset` before
joining. Includes both the join keys and the variable being added.

#### `by_vars`

Comma-separated quoted column names used as join keys, ready for use in
`dplyr::left_join(by = c(...))`.

#### `needs_rename`

`TRUE` when the variable name in the source domain differs from the
desired output name. When `TRUE`, a
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
call is appended.

#### `output_var` / `var_to_add`

`var_to_add` is the column name as it exists in `join_dataset`.
`output_var` is the desired column name in `self`. When `needs_rename`
is `FALSE` they are equal.

## mighty_write_data

Rendered at the end of every ADaM program. Sorts rows, selects columns
in specification order, and persists the dataset via the connector.

### Example

For the ADSL specification from the `mighty_read_data` example, mighty
builds:

``` r

list(
  self = "ADSL",
  file_ext = "parquet",
  row_order_vars = "USUBJID,\nSTUDYID",
  keep_vars = "USUBJID,\nSTUDYID,\nARM"
)
```

`keep_vars` is always pre-formatted as a `,\n`-separated string.

> **Note**
>
> When mighty is run with source data available for validation, columns
> that cannot be derived due to missing source data are prefixed with
> `#` in the formatted string, commenting them out of the
> [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
> call. This allows the generated program to run and produce a partial
> dataset rather than failing outright. This behaviour is part of
> mighty’s missing data handling and does not affect the template
> itself.

``` r

list(
  self = "ADLB",
  file_ext = "parquet",
  row_order_vars = "USUBJID,\nSTUDYID",
  keep_vars = "USUBJID,\nSTUDYID,\nPARAMCD,\nAVAL"
)
```

The template and two rendered versions — one with a short column list
and one with a longer list — can be expanded below.

    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_write_data" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_write_data" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
    #> → Using cached repo "NovoNordisk-OpenSource/mighty.standards@dev/internal-components"
    #> → Found "mighty_write_data" in "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"

Standard template and rendered output

**Template**

    #' @title Persist ADaM dataset
    #' @description Sorts rows by the domain's primary keys, selects and orders
    #'   columns according to the YAML specification, and persists the dataset via
    #'   the connector. Rendered once at the end of every ADaM program.
    #' @type internal
    #' @param self Character string. Name of the ADaM domain to persist (e.g. `"ADSL"`, `"ADLB"`).
    #' @param row_order_vars Character string or `NULL`. Primary key columns used to sort rows, formatted as a `,\n`-separated string. `NULL` when no row order is defined — the sort block is skipped.
    #' @param keep_vars Character string or `NULL`. Output columns in specification order, formatted as a `,\n`-separated string. `NULL` for intermediate programs in multi-program domains — the select block is skipped. Columns not yet available are prefixed with `# `.
    #' @param file_ext Character string. File extension for the persisted dataset. Defaults to `"parquet"`.
    #' @code
    {{#row_order_vars}}
    # Sort rows by primary key
    {{{self}}} <- {{{self}}} |> dplyr::arrange({{{row_order_vars}}})

    {{/row_order_vars}}
    {{#keep_vars}}
    # Sort columns
    {{{self}}} <- {{{self}}} |> dplyr::select({{{keep_vars}}})

    {{/keep_vars}}
    # Save ADaM table
    cnt$adam$write_cnt({{{self}}}, tolower("{{{self}}}.{{{file_ext}}}"), overwrite = TRUE)

**Rendered output — short column list**

``` r
# Sort rows by primary key
ADSL <- ADSL |> dplyr::arrange(USUBJID,
STUDYID)

# Sort columns
ADSL <- ADSL |> dplyr::select(USUBJID,
STUDYID,
ARM)

# Save ADaM table
cnt$adam$write_cnt(ADSL, tolower("ADSL.parquet"), overwrite = TRUE)
```

**Rendered output — long column list**

``` r
# Sort rows by primary key
ADLB <- ADLB |> dplyr::arrange(USUBJID,
STUDYID)

# Sort columns
ADLB <- ADLB |> dplyr::select(USUBJID,
STUDYID,
PARAMCD,
AVAL)

# Save ADaM table
cnt$adam$write_cnt(ADLB, tolower("ADLB.parquet"), overwrite = TRUE)
```

### Parameters

#### `self`

The name of the domain being built (e.g. `"ADSL"`, `"ADLB"`).

#### `file_ext`

File extension for the persisted dataset. Defaults to `"parquet"`. Will
be configurable via `_mighty.yml` (global default) and per-domain YAML
(`table_metadata`) in a future release.

#### `row_order_vars`

The domain’s primary keys, formatted as a `,\n`-separated string. `NULL`
when the domain has no primary keys — whisker treats `NULL` as falsy so
the sort block is suppressed.

#### `keep_vars`

The output columns in specification order. `NULL` for intermediate
programs in multi-program domains — the template uses `keep_vars`
directly as the section condition, so a `NULL` value suppresses the
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
call. Always passed as a `,\n`-separated string. See the callout above
regarding columns that may be prefixed with `#`.
