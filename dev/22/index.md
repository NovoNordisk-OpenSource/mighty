# Overview

The mighty framework introduces a declarative paradigm to building ADaM
datasets, emphasizing *what you need* rather than *how to create it*.
This shift enables users to focus on the structure, derivations, and
intent of your datasets while abstracting away tedious implementation
details.

**Important** mighty is a work-in-progress, so expect the documentation
and functionality to change.

## Why mighty?

### Declarative Specification

With mighty, you specify exactly what belongs in your ADaM
datasets—including columns, derivations, and row transformations—and
point to existing code implementations (or your own custom ones) that
handle the actual data manipulations. Mighty orchestrates and compiles
these instructions into executable ADaM programs.

### Single Source of Truth

Mighty ensures metadata required for your define.xml always matches the
code implementation. Components and specifications are consolidated into
a single representation, ensuring any changes automatically propagate
throughout your ADaM programs and keeping documentation synchronized
with implementation.

### Dependency Analysis

Mighty tracks column and row dependencies for every derivation,
analyzing the dependency topology among actions to determine execution
order. This ensures that derived variables are created in the correct
sequence, with prerequisite calculations completed before dependent
operations. Dependency validation alerts you when required columns are
missing, or if cyclic dependencies are detected, to aide debugging.

### Missing Data Analysis

Identification of discrepancies between specified ADaM requirements and
available SDTM/ADaM data, enables generation of executable programs that
gracefully adapt to current data availability. See
[`vignette("missing_data_analysis")`](https://novonordisk-opensource.github.io/mighty/articles/missing_data_analysis.md)
for more details.

## Example: Deriving COMPLSFL in ADSL

To illustrate mighty’s declarative paradigm, consider the following
example where we define and derive the variable COMPLSFL for the ADSL
domain. This column indicates whether the subject’s end-of-study status
is “COMPLETED.”

``` yml
id: ADSL

keys:
  - USUBJID
  - STUDYID

population:
  base:
    - domain: DM
      depends: NA
      filter: NA
  global:
    - filter: NA
      depends: NA

columns:
  - id: USUBJID

  - id: STUDYID

  - id: EOSSTT

  - id: COMPLSFL
    component:
      id: der_complsfl
```

This specification uses the mighty.metadata format with four main
sections:

- **id**: The ADaM domain name (ADSL)
- **keys**: Primary key variables for the dataset
- **population**: Defines which SDTM domains form the base population
- **columns**: An array of column definitions with their derivation
  methods

The `COMPLSFL` column points to the code component called
`der_complsfl`.

### Code components

Code components can either be validated standard components or custom
(un-validated) components. In this example `der_complsfl` is a
**standard component**. We can see this is the case because we simply
refer to the component name in the `id` field.

**Custom components** need to be specified with a path to their
location:

``` yml
  - id: COMPLSFL
    component:
      id: path/to/custom/component/der_complsfl.R
```

Notice the `.R` extension. Custom components are simply R scripts that
adhere to a few simple rules. See
[`vignette("code_components")`](https://novonordisk-opensource.github.io/mighty/articles/code_components.md)
for more details.

As mighty matures, more of these components will be pre-defined and
ready for use, so fewer components would need to be defined by the user.
Standard components are provided by
[`mighty.standards`](https://github.com/NovoNordisk-OpenSource/mighty.standards),
while
[`mighty.component`](https://github.com/NovoNordisk-OpenSource/mighty.component)
provides utilities for rendering both standard and custom components.

See
[`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md)
for more details on the ADaM Specification data model.

For now, let’s assume `der_complsfl` doesn’t exist and we have to write
it ourselves:

``` r

#' @title Completion Status Flag Calculation
#'
#' @description This script adds a `COMPLSFL` column to the input data frame,
#' indicating whether a subject's end-of-study status is marked as "COMPLETED".
#'
#' @type derivation
#'
#' @depends ADSL EOSSTT
#'
#' @outputs COMPLSFL
#' @code

# Add a new column COMPLSFL based on the EOSSTT value
ADSL <- ADSL |> dplyr::mutate(
  COMPLSFL = dplyr::case_when(
    EOSSTT == "COMPLETED" ~ "Y",  # Mark "Y" if EOSSTT is "COMPLETED"
    TRUE                  ~ "N"   # Mark "N" otherwise
  )
)
```

## How it works

mighty processes three key inputs to automatically generate ADaM
programs:

1.  **ADaM domain specifications** - Define structure and requirements
    for analysis datasets
2.  **Code components** - Reusable building blocks referenced in
    specifications
3.  **Data availability context** - Information about which SDTM/ADaM
    domains and columns are currently available

The package intelligently orchestrates the execution order of
derivations, parameters, and imputations, organizing these operations
into structured, production-ready ADaM programs.

> **Note:** Code components can be sourced from mighty.standards
> (processed via mighty.component) or defined as custom user components,
> depending on project requirements.

See
[`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md)
for more details on ADaM specifications,
[`vignette("mighty_config")`](https://novonordisk-opensource.github.io/mighty/articles/mighty_config.md)
for study metadata configuration, and
[`vignette("connect_to_data")`](https://novonordisk-opensource.github.io/mighty/articles/connect_to_data.md)
for more details on how source data is retrieved in the rendered
programs.

## The mighty ecosystem

The mighty framework consists of these complementary packages:

- **mighty** - Core orchestration engine
- **mighty.metadata** - ADaM metadata management in YAML format
- **mighty.component** - Utilities enabling mighty to handle code
  components
- **mighty.standards** - Standard component library with pre-built,
  validated derivations (coming later)
- **mighty.ai** - AI-powered helper for the mighty ecosystem (coming
  later)
- **mighty.editor** - An enhanced editor for handling ADaM
  specifications and components (coming later)
- **mighty.toolbox** - A toolbox for define.xml generation and
  validation of ADaM related deliverables according to CDISC and
  regulatory requirements

> **Note:** All packages in the mighty ecosystem are currently under
> active development and are subject to change. Package names, features,
> and functionality may evolve as the ecosystem matures.

## Installation

Install the development version from GitHub:

``` r

# Using pak (recommended)
pak::pak("NovoNordisk-OpenSource/mighty")
```
