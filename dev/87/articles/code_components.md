# Code Components

## Overview

Code components are the building blocks that implement the actual
derivations, transformations, and calculations in your ADaM datasets.
Components come in two technical formats that determine how mighty
processes and executes them:

- **R script components** - Plain executable R code (`.R` files)
- **Mustache template components** - Parameterizable templates
  (`.mustache` files)

Component metadata parsing and Mustache rendering are handled by the
[mighty.component](https://github.com/NovoNordisk-OpenSource/mighty.component)
package. This vignette describes the metadata format and file structure
that components must follow; mighty.component is what mighty uses under
the hood to read and render them. \# Component types Beyond their
technical format, components are classified by what they do to your
data. `@type` takes one of four values: `column`, `row`, `parameter`, or
`internal`.

**Column components** (`@type column`) create new columns in your
dataset. They take existing columns as inputs and derive new variables.
For example, calculating an age group from age, or computing a relative
day from two dates. Column components:

- Always create one or more new columns (specified in `@outputs`)
- Can depend on columns from multiple datasets
- Build on top of each other - one component’s output can be another’s
  input
- Are the most common type of component
- Are referenced in the `columns` section of your YAML ADaM
  specification

**Row components** (`@type row`) add new rows to your dataset or modify
existing rows. For example, computing summary statistics that create new
observation rows. Row components:

- Add new records to the dataset
- May modify column values in the newly created rows
- May modify existing rows (i.e. with a subset operation)
- List any modified columns in `@outputs`
- Do not create new columns - they only populate values in existing
  column structures
- Are referenced in the `rows` section of your YAML ADaM specification

**Parameter components** (`@type parameter`) create BDS parameter
records, such as adding a new PARAMCD to a BDS dataset. They are
processed the same way as row components. Parameter components:

- Add new parameter records to the dataset
- May modify column values in the newly created rows
- List any modified columns in `@outputs`
- Are referenced in the `parameters` section of your YAML ADaM
  specification

**Internal components** (`@type internal`) implement mighty’s own
internal machinery, such as reading source data or initializing a
domain. They are not referenced from a column, row, or parameter section
of a YAML ADaM specification, and are not typically written by end
users.

The key distinction between the user-facing types: column components
expand your dataset horizontally (more variables), while row and
parameter components expand it vertically (more records).

## Component Structure

A component is one file with two parts. Here is a complete R component:

``` r

#' @title Age group
#' @description Groups subjects into under 65 and 65 or over
#' @type column
#' @origin Derived
#' @method AGE below 65 vs 65 and over.
#' @depends ADSL AGE
#' @outputs AGEGR1
#' @code
ADSL <- ADSL |>
  dplyr::mutate(AGEGR1 = dplyr::if_else(AGE < 65, "<65", ">=65"))
```

- **Header**: the lines starting with `#'`. They describe the component
  and say which columns it needs (`@depends`) and creates (`@outputs`).
  Each tag is described below.
- **Body**: the R code after `@code`. mighty inserts it into the
  generated program.

The format you choose depends on whether your derivation logic needs
parameterization. R scripts execute fixed logic, while Mustache
templates allow you to pass parameters from your ADaM specification to
customize behavior.

## Metadata Header

All components use the same metadata header structure with
roxygen2-style tags. Mighty extracts this metadata to understand
dependencies, outputs, and how to orchestrate components.

### Metadata Tags

#### `@title`

Short descriptive title for the component.

``` r

#' @title Age Group Classification
```

#### `@description`

Detailed explanation of what the component does.

``` r

#' @description Categorizes subjects into age groups based on AGE variable
```

#### `@param`

*Only for Mustache template components* - Parameters to pass to the
template, populated from the `with` section of the ADaM specification.
See
[`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md).

``` r

#' @param domain Name of the ADaM domain
#' @param cut_points Vector of age cut points
```

#### `@type`

Component type: `"column"`, `"row"`, `"parameter"`, or `"internal"`.

``` r

#' @type column
```

#### `@origin`

CDISC origin of the column, one of `"Assigned"`, `"Collected"`,
`"Derived"`, `"Not Available"`, `"Other"`, `"Predecessor"`, or
`"Protocol"`. Used for define.xml generation.

``` r

#' @origin Derived
```

#### `@method`

Description of how the column is derived, in prose. Used as the method
description for define.xml generation.

``` r

#' @method Age cut into the groups < 18, 18-64, 65-74 and >= 75 years.
```

#### `@depends`

Input dependencies listing the required columns. Format:
`DOMAIN COLUMN1`

Multiple depends are listed on separate lines:

``` r

#' @depends ADSL AGE
#' @depends ADLB LBSTRESN
```

The generated program reads only the columns that the specification or a
component declares. List every column your code uses, including source
columns that are not in the final dataset (see [Source columns not in
the final dataset](#source-columns-not-in-the-final-dataset)). mighty
also uses `@depends` to run components in the right order.

#### `@outputs`

If the component is a column derivation, this specifies which column(s)
the component creates. If the component is a row or parameter operation,
this specifies which column(s) the component modifies

Multiple outputs are listed on separate lines.

``` r

#' @outputs AGE_GRP1
#' @outputs COL1
```

Mighty uses this to track what each component produces or modifies.

#### `@code`

Marker indicating where the executable code (the component body) begins.
All code must come after this tag.

``` r

#' @code

# Your R code here
ADSL <- ADSL |> dplyr::mutate(...)
```

## R Script Components

R script components contain plain executable R code with fixed
derivation logic. They are saved as `.R` files and execute the same way
every time they are called.

### When to Use

Use R script components when:

- Derivation logic is fixed and does not need parameterization
- The component is specific to a single use case
- You want straightforward, readable code without template syntax

### Code Structure

- All executable code must come after the `@code` tag. mighty does not
  change anything in your R code.
- Use roxygen2 comment style (`#'`) for metadata
- Regular R comments (`#`) can be used within the code
- Each dataset is a data frame named after its domain (`ADSL`, `DM`,
  `LB`). Your code must update the data frame of the domain it belongs
  to, e.g. `ADSL <- ADSL |> dplyr::mutate(...)`.
- Other domains listed in `@depends` are available as data frames under
  their own names. For example, an ADLB component with
  `@depends LB LBTEST` can read the SDTM `LB` data frame and join it
  into `ADLB`.
- A column component must not change the number of rows.

### Example: Simple Column Derivation

``` r

#' @title Age Group Classification
#' @description Categorizes subjects into age groups based on AGE variable
#' @type column
#' @origin Derived
#' @method Age cut into the groups < 18, 18-64, 65-74 and >= 75 years.
#' @depends ADSL AGE
#' @outputs AGE_GRP1
#' @code

ADSL <- ADSL |>
  # This is an example comment
  dplyr::mutate(
    AGE_GRP1 = cut(
      AGE,
      breaks = c(-Inf, 18, 65, 75, Inf),
      labels = c("< 18 years", "18 - 64 years", "65 - 74 years", ">= 75 years"),
      right = FALSE
    )
  )
```

### Example: Dependent Derivation

Components can depend on other derived columns:

``` r

#' @title Age Category Classification
#' @description Further categorizes age groups into expected vs unexpected ranges
#' @type column
#' @origin Derived
#' @method Age groups < 18 and >= 75 years are flagged as outside the expected range.
#' @depends ADSL AGE_GRP1
#' @outputs AGE_CAT1
#' @code

ADSL <- ADSL |>
  dplyr::mutate(
    AGE_CAT1 = dplyr::case_when(
      AGE_GRP1 %in% c("< 18 years", ">= 75 years") ~ "Outside expected range",
      TRUE ~ "Inside expected range"
    )
  )
```

### Example: Multiple Dependencies

Components can depend on multiple columns from multiple datasets:

``` r

#' @title Minimum AVAL per Subject
#' @description Derives the minimum lab value for each subject from ADLB
#' @type column
#' @origin Derived
#' @method Lowest AVAL recorded for the subject in ADLB.
#' @depends ADSL USUBJID
#' @depends ADLB USUBJID
#' @depends ADLB AVAL
#' @outputs MIN_AVAL
#' @code

ADSL <- ADSL |>
  dplyr::left_join(
    ADLB |>
      dplyr::select(USUBJID, AVAL) |>
      dplyr::group_by(USUBJID) |>
      dplyr::summarise(MIN_AVAL = min(AVAL, na.rm = TRUE)) |>
      dplyr::ungroup(),
    by = "USUBJID"
  )
```

## Mustache Template Components

Mustache template components use [Mustache
syntax](https://mustache.github.io/mustache.5.html) to enable
parameterization. They are saved as `.mustache` files and allow the same
component logic to be reused with different parameters.

### When to Use

Use Mustache template components when:

- You want to reuse a component across multiple columns, datasets,
  trials, or projects
- The same derivation logic needs to work with different parameters

### Template Syntax

Mustache templates use triple curly braces `{{{variable}}}` as
placeholders that get replaced with actual values when mighty renders
the component. Triple braces avoid HTML escaping, which would corrupt R
operators such as `<-` and `>` in generated code. See [Template
syntax](https://novonordisk-opensource.github.io/mighty/articles/special_components.html#template-syntax)
in the internal components vignette for a full explanation of double
vs. triple mustache syntax.

- Parameters are specified in the `with:` section of your YAML
  specification
- Both metadata and code sections can contain placeholders

### Code Structure

- Metadata header uses `@param` tags to document expected parameters
- Placeholders can appear in `@depends`, `@outputs`, and the code body
- Mighty (via mighty.component) renders the template before execution,
  replacing all placeholders
- The rendered output is plain R code
- mighty passes only the values under `with:`. To use `{{{domain}}}`,
  pass `domain` there (see the example below).

### Example: Parameterized Column Derivation

**Template file: `relative_day.mustache`**

``` mustache
#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#'
#' @param domain Name of ADaM domain
#' @param variable Name of new variable to create
#' @param date Name of date variable to use
#' @type column
#' @origin Derived
#' @method Number of days from TRTSDT to {{{date}}}, counting both endpoints.
#' @depends {{{domain}}} {{{date}}}
#' @depends {{{domain}}} TRTSDT
#' @outputs {{{variable}}}
#' @code
{{{domain}}} <- {{{domain}}} |>
  dplyr::mutate(
    {{{variable}}} = admiral::compute_duration(
      start_date = TRTSDT,
      end_date = {{{date}}},
      in_unit = "days",
      out_unit = "days",
      add_one = TRUE
    )
  )
```

**Usage in ADaM specification:**

``` yml
columns:
  - id: ADYDT
    component:
      id: relative_day.mustache
      with:
        domain: ADVS
        date: ADT
        variable: ADYDT
```

**Rendered output (what mighty sees):**

``` r

#' @title Analysis relative day
#' @description
#' Derives the relative day compared to the treatment start date.
#'
#' @param domain Name of ADaM domain
#' @param variable Name of new variable to create
#' @param date Name of date variable to use
#' @type column
#' @origin Derived
#' @method Number of days from TRTSDT to ADT, counting both endpoints.
#' @depends ADVS ADT
#' @depends ADVS TRTSDT
#' @outputs ADYDT
#' @code
ADVS <- ADVS |>
  dplyr::mutate(
    ADYDT = admiral::compute_duration(
      start_date = TRTSDT,
      end_date = ADT,
      in_unit = "days",
      out_unit = "days",
      add_one = TRUE
    )
  )
```

### Example: Reusing Templates with Different Parameters

The same Mustache template can be used multiple times with different
parameters:

``` yml
columns:
  - id: ADYDT
    component:
      id: relative_day.mustache
      with:
        domain: ADVS
        date: ADT
        variable: ADYDT


  - id: ASTDY
    component:
      id: relative_day.mustache
      with:
        domain: ADVS
        date: ASTDT
        variable: ASTDY
```

For the full rules on how mighty handles component reuse in ADaM
specifications — including restrictions on overlapping outputs — see
[Component reuse
rules](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.html#component-reuse-rules).

## Source columns not in the final dataset

The YAML specification contains only columns that appear in the final
ADaM dataset (see [No intermediate
columns](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.html#no-intermediate-columns)).
Components often need source columns that are not in the final dataset —
for example, an SDTM character date used to derive an analysis date.

When a component declares `@depends DOMAIN COLUMN`, mighty adds that
column to the data-reading step of the generated program. The column is
available inside the component’s code without appearing in the YAML
`columns` section. This is the intended mechanism for accessing source
data that does not belong in the final output.

For example, a component that derives `ASTDT` and `ASTDTF` from the SDTM
character date `LBDTC`:

``` r

#' @title Convert DTC to DT
#' @description Converts DTC to date (DT) format and computes the corresponding date flag (DTF) for the ADLB dataset.
#' @type column
#' @origin Derived
#' @method Analysis start date and date flag derived from the SDTM LBDTC character date.
#' @depends LB USUBJID
#' @depends LB STUDYID
#' @depends LB LBSEQ
#' @depends LB LBDTC
#' @depends ADLB USUBJID
#' @depends ADLB STUDYID
#' @depends ADLB LBSEQ
#' @outputs ASTDT
#' @outputs ASTDTF
#' @code
ADLB <- ADLB |>
  dplyr::left_join(
    LB |>
      dplyr::mutate(
        ASTDT = admiral::convert_dtc_to_dt(
          dtc = LBDTC,
          highest_imputation = "M",
          date_imputation = "first"
        ),
        ASTDTF = admiral::compute_dtf(
          dtc = LBDTC,
          dt = ASTDT
        )
      ),
    by = c("USUBJID", "STUDYID", "LBSEQ")
  )
 
```

Mighty reads `LBDTC` from the source domain because the component
declares `@depends LB LBDTC`, even though `LBDTC` has no entry in the
YAML `columns` section.

This approach may cause the same source column to be joined more than
once across different components. This is an accepted trade-off:

- Multiple columns can share a single component. If several derived
  columns need the same source data, group them in one component to
  avoid redundant reads.
- Compute time is rarely the bottleneck in ADaM programming. Mighty
  optimizes for development time, not compute time.
- Self-contained components are easier to test and reuse independently.

## Standard vs. custom components

The classification of a component as either Standard or Custom is
determined only by the governance of standard components. Mighty treats
all components the same. Standard components and custom components can
be either `.mustache` files or `.R` files.

### How mighty finds a component

Mighty looks up the `id` of a component in the repositories listed under
`repos` in `_mighty.yml`. To use a custom component, add the directory
that contains it to `repos`. See
[`vignette("mighty_config")`](https://novonordisk-opensource.github.io/mighty/articles/mighty_config.md)
for the lookup rules.

## Internal components

A small number of internal components provide core functionality managed
by mighty itself:

- Data I/O operations (reading from connectors, writing outputs)
- Domain initialization and filtering
- Join operations based on primary keys defined in `_mighty.yml`
  `external_data`
- Not directly referenced by users in specifications

These are automatically generated and inserted by mighty as part of the
program generation process.
