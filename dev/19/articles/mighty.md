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

``` yml
external_data:
  - id: DM
    keys: [USUBJID, STUDYID]
  - id: LB
    keys: [USUBJID, STUDYID, LBSEQ]
repos:
  - "NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
  - "."
```

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

``` yml
id: ADSL
label: Subject Level Analysis Dataset
class: SUBJECT LEVEL ANALYSIS DATASET
structure: One record per subject
keys: [USUBJID, STUDYID]

population:
  base:
    - domain: DM
      depends:
        - AGE
      filter: '!is.na(AGE)'
  global:
    - filter: NA
      depends:
        - NA

columns:
  - id: USUBJID

  - id: STUDYID

  - id: ARM

  - id: PLANNED_ARM
    method: ARM

  - id: AGE

  - id: AGE_GRP1
    component:
      id: "age_group_01.R"
```

### Define your custom component

The column `AGE_GRP1` in ADSL is derived by using a custom component
that contains the logic to process the ADSL table by deriving the
additional column `AGE_GRP1`.

**age_group_01.R**

``` r

#' @title Age group 01
#' @description Grouping of age
#' @type column
#' @depends ADSL AGE
#' @outputs AGE_GRP1
#' @returns `ADSL`
#' @code
ADSL <- ADSL |>
  dplyr::mutate(
    AGE_GRP1 = cut(
      AGE,
      breaks = c(-Inf, 18, 65, 75, Inf),
      labels = c("< 18 years", "18 - 64 years", "65 - 74 years", ">= 75 years"),
      right = FALSE
    )
  )
```

### Render the ADaM program

Call mighty to consolidate, analyze and render your ADSL program:

**example_01_main.R**

``` r

# ADSL with 1 base domain
#   - 4 predecessors
#   - 1 renamed predecessor
#   - 1 derivation

# Load mighty
library(mighty)

# Generate ADaM program
results <- mighty::generate_adam_code(
  adam_specifications = "study_dir",
  path_connector_config = "_connector.yml"
)

# Print rendered ADaM program
results$programs$`1_ADSL` |> cat()
```

Printing the rendered ADaM program yields a complete program for
generating ADSL based on the specifications

``` r

# ADSL-1-read_data -------------------------------------------------------------
cnt <- connector::connect(config = "/tmp/RtmpCMAflt/mighty_example_study/_connector.yml")
  DM <- cnt$sdtm$read_cnt(tolower('DM')) |>
  dplyr::select(AGE, ARM, STUDYID, USUBJID)

# ADSL-init_domain -------------------------------------------------------------
DM <- DM |>
  dplyr::mutate(SRC_ = "DM")

ADSL <- DM |>
    dplyr::select(AGE, ARM, STUDYID, USUBJID, SRC_) |>
    admiral::convert_blanks_to_na()

# ADSL-filter_domain -----------------------------------------------------------
ADSL <- ADSL |>
  dplyr::filter((SRC_ == 'DM' & !is.na(AGE))) |>
  dplyr::select(-SRC_)

ADSL <- ADSL |>
  dplyr::select(AGE, ARM, STUDYID, USUBJID)


# ADSL-AGE_GRP1 ----------------------------------------------------------------
ADSL <- ADSL |>
  dplyr::mutate(
    AGE_GRP1 = cut(
      AGE,
      breaks = c(-Inf, 18, 65, 75, Inf),
      labels = c("< 18 years", "18 - 64 years", "65 - 74 years", ">= 75 years"),
      right = FALSE
    )
  )

# ADSL-PLANNED_ARM -------------------------------------------------------------
ADSL <- ADSL |> dplyr::mutate(PLANNED_ARM = ARM)

# ADSL-1-write_data ------------------------------------------------------------
# Sort rows by primary key
ADSL <- ADSL |> dplyr::arrange(USUBJID,
STUDYID)

# Sort columns
ADSL <- ADSL |> dplyr::select(USUBJID,
STUDYID,
ARM,
PLANNED_ARM,
AGE,
AGE_GRP1)

# Save ADaM table
cnt$adam$write_cnt(ADSL, tolower("ADSL.parquet"), overwrite = TRUE)
```
