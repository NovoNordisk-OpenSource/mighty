
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mighty

<!-- badges: start -->

<!-- badges: end -->

Mighty is the main package that powers the mightyverse.

The mightyverse introduces a declarative paradigm to building ADaM
datasets, emphasizing what you need rather than how to create it. This
shift enables you to focus on the structure, derivations, and intent of
your datasets while abstracting away tedious processes.

# Why mighty?

## Declarative Specification

With mighty, you specify exactly what belongs in your ADaM
datasets—including columns, derivations, and row transformations—and
point to existing code implementations (or your own custom ones) that
handle the actual data manipulations. Mighty orchestrates and compiles
these instructions into executable ADaM programs, optimizing for
accuracy, traceability, and reproducibility.

## Automation and Synchronization

Let mighty take care of the heavy lifting:

- **Define.XML Alignment:** Ensures the metadata required for your
  define.xml always matches the current implementation of your ADaM
  program. Any changes to your ADaM specifications or code components
  automatically propagate to the compiled program.
- **Dependency Tracking:** Tracks column and row dependencies for every
  derivation. You’ll receive real-time alerts when a dependency is
  missing, reducing debugging time and ensuring seamless integrations.
- **Streamlined Compilation:** Mighty compiles your ADaM programs while
  maintaining standards for clarity and functionality

# Example Workflow

Here’s a simple overview of how mighty works:

1.  Define your ADaM table in a YAML file with sections for table
    metadata, initialization, column specifications, and row operations.
2.  Write or reference R functions as code components for derivations
    and row manipulations.
3.  Use mighty to compile the ADaM program based on your specifications
    and code components.
4.  Execute the program in your preferred environment for dataset
    creation.

# Example: Deriving COMPLSFL in ADSL

To illustrate mighty’s declarative paradigm, consider the following
example where we define and derive the variable COMPLSFL for the ADSL
domain. This column indicates whether the subject’s end-of-study status
is “COMPLETED.”

``` yml
table_metadata:
  table: ADSL

init:
  base_domains:
    - DM
  filter_domain:
    - DM: NA
  filter_global: NA
  filter_depend_cols: NA

column_metadata:
  USUBJID:
    source: USUBJID
  EOSSTT:
    source: EOSSTT
  COMPLSFL:
    code_id: der_complsfl
```

This specification points to the code component called `der_complsfl`.

# Code components

Code components can either be validated standard components or custom
(un-validated) components. In this example `der_comflsfl` is a standard
component. We can see this is the case because we simply refer to the
name of the component.

Custom components need to be specified with a path to their location:

``` yml
  COMPLSFL:
    code_id: path/to/custom/component/der_complsfl.R
```

Notice the `.R` extension. Custom components are simply R functiond that
adhere to a few simple rules. See \[\] for more details

As mighty matures, more of these functions will be pre-defined and ready
for use, so nothing more would need to be done by the user. For more
information, please refer to
[`mighty.component`](https://github.com/NN-OpenSource/mighty.component).

See \[\] section for more details on the ADaM Specification data model.

For now, let’s assume `der_complsfl` doesn’t exist and we have to write
it ourselves:

``` r
#' @title Completion Status Flag Calculation
#'
#' @description This function adds a `COMPLSFL` column to the input data frame,
#' indicating whether a subject's end-of-study status is marked as "COMPLETED".
#' 
#' @type derivation
#' 
#' @depends .self EOSSTT
#' 
#' @outputs COMPLSFL
#' 
#' @returns `.self`
#' 
#' @export
complsfl <- function(.self) {
  # Add a new column COMPLSFL based on the EOSSTT value
  .self <- .self |> dplyr::mutate(
    COMPLSFL = dplyr::case_when(
      EOSSTT == "COMPLETED" ~ "Y",  # Mark "Y" if EOSSTT is "COMPLETED"
      TRUE                   ~ "N"    # Mark "N" otherwise
    )
  )
  return(.self)  
}
```
