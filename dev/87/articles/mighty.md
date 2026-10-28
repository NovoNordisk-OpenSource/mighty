# Get started

## Your first mighty ADaM program

In this minimal example you build ADSL with mighty. You will:

1.  set up the study config (`_mighty.yml`),
2.  describe ADSL in a specification (`adsl.yml`),
3.  write one custom component for a derived column,
4.  call
    [`generate_adam_code()`](https://novonordisk-opensource.github.io/mighty/reference/generate_adam_code.md),
    which returns the R program that builds ADSL.

### Key terms

- **Domain**: one dataset, e.g. `DM` (SDTM) or `ADSL` (ADaM).
- **Specification**: a YAML file describing one ADaM domain you want
  mighty to build, e.g. `adsl.yml`. mighty generates the program for
  every domain that has a specification.
- **External domain**: a dataset the program reads but mighty doesn’t
  build: SDTM, metadata, or ADaM made elsewhere.
- **Keys**: the columns that identify one row in a table. mighty uses
  them to join domains. A generated domain lists its keys in its
  specification; external domains list them in `_mighty.yml`.
- **Component**: an R script or Mustache template with the code for one
  derivation, e.g. `age_group_01.R`.
- **`repos`**: the folders or GitHub repositories where mighty looks for
  components, listed in `_mighty.yml`.
- **define.xml**: the document that describes each ADaM column for
  regulatory submission. mighty does not create it; mighty.toolbox
  builds it from the same specifications.

### Study directory guidelines

A mighty study looks like this:

    my_study/                 <- working directory when running example_01_main.R
    ├── example_01_main.R
    ├── _connector.yml
    ├── study_dir/            <- mighty specifications
    │   ├── _mighty.yml
    │   ├── _study.yml
    │   └── adsl.yml
    └── components/           <- "./components" in repos
        └── age_group_01.R

`study_dir/` holds only YAML files:

- one specification per ADaM domain (e.g. `adsl.yml`)
- `_mighty.yml`: [framework
  configuration](https://novonordisk-opensource.github.io/mighty/articles/mighty_config.md)
  (keys of external datasets and component repositories)
- `_study.yml`: study-level properties (optional, used by mighty.toolbox
  for define.xml)
- `_documents.yml`, `_codelists.yml`: optional, read by mighty.metadata

mighty reads every other `.yml` file in `study_dir/` as a specification.
Keep other files outside it: the [connector
config](https://novonordisk-opensource.github.io/connector/)
(`_connector.yml`), which tells the generated programs where your data
is; your custom components; and the folder you save the generated
programs to.

### Set up your framework configuration file

`_mighty.yml` holds the study-wide settings. It has two parts:

- `external_data`: the keys of each [external domain](#key-terms) the
  programs read. mighty uses them when it joins domains. Here that is
  `DM`, the source of ADSL. ADSL’s own keys are in `adsl.yml`.
- `repos`: where mighty looks for components (see below).

The `repos` field lists where mighty looks for code components. The
first entry points to the standard components repo; the second is a
local directory with the study-specific custom components.

**\_mighty.yml**

``` yml
external_data:
  - id: DM
    keys: [USUBJID, STUDYID]
  - id: LB
    keys: [USUBJID, STUDYID, LBSEQ]
repos:
  - "github::NovoNordisk-OpenSource/mighty.standards/components@dev/internal-components"
  - "./components"
```

### Set up your ADaM specification

The specification has a header describing ADSL, a `population` section
saying the rows come from `DM` (keeping subjects with a known `AGE`),
and a `columns` list with every column of the final dataset:

- `USUBJID`, `STUDYID`, `ARM`, `AGE`: listed as an `id`, so they’re kept
  from `DM` as they are.
- `PLANNED_ARM`: `method: ADSL.ARM` makes a copy of `ARM` under a new
  name.
- `AGE_GRP1`: `component` means it’s derived by the custom component
  `age_group_01.R` (see next section).

Listing only columns that belong in the final dataset is a good practice
that helps in keeping things tidy. See
[`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md)
for everything a specification can contain.

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
    method: ADSL.ARM

  - id: AGE

  - id: AGE_GRP1
    component:
      id: "age_group_01.R"
```

### Define your custom component

The column `AGE_GRP1` in ADSL is derived by using a custom component
that contains the R logic to process the ADSL table by deriving the
additional column `AGE_GRP1`.

Save the component in the `components` directory listed under `repos`.
The ADaM specification refers to it by its file name, `age_group_01.R`.

Relative paths in `repos` are resolved against the working directory,
not against the location of `_mighty.yml`. This example assumes R is
started in `my_study/`, the folder that contains both `study_dir/` and
`components/` (see the tree under [Study directory
guidelines](#study-directory-guidelines)).

**age_group_01.R**

``` r

#' @title Age group 01
#' @description Grouping of age
#' @type column
#' @origin Derived
#' @method Age cut into the groups < 18, 18-64, 65-74 and >= 75 years.
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

The `#'` header tells mighty what the component needs
(`@depends ADSL AGE`) and what it creates (`@outputs AGE_GRP1`). The
code after `@code` is inserted into the ADSL program as it is. See
[`vignette("code_components")`](https://novonordisk-opensource.github.io/mighty/articles/code_components.md)
for all tags.

### Render the ADaM program

Call mighty to consolidate, analyze and render your ADSL program:

`path_connector_config` is the path to your connector config. mighty
writes this path into the generated programs, which read the file when
they run.

**example_01_main.R**

``` r

# ADSL with 1 base domain (DM)
#   - 4 columns kept from DM
#   - 1 copied column (PLANNED_ARM)
#   - 1 derived column (AGE_GRP1)

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

[`generate_adam_code()`](https://novonordisk-opensource.github.io/mighty/reference/generate_adam_code.md)
returns a list. Its `programs` element holds the generated R programs,
each one a character string. Each name starts with the order to run the
programs in, so here there is a single program, `1_ADSL`. When a study
has several domains, a domain that uses ADSL comes after it
(e.g. `2_ADAE`). If ADSL itself needs columns from another domain, ADSL
is split into two programs (e.g. `1_ADSL`, `2_ADLB`, `3_ADSL`). Printing
`1_ADSL` shows the full program that builds ADSL:

    #> → No `_documents.yml` file found
    #> → No `_codelists.yml` file found
    #> → Downloading repo "NovoNordisk-OpenSource/mighty.standards@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "age_group_01.R" in "local::./components"
    #> → Found "mighty_read_data" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_init_domain" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_filter_domain" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "age_group_01.R" in "local::./components"
    #> → Found "mighty_col_mutate" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_write_data" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_read_data" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_init_domain" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_filter_domain" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "age_group_01.R" in "local::./components"
    #> → Found "mighty_col_mutate" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    #> → Found "mighty_write_data" in "github::NovoNordisk-OpenSource/mighty.standards/components@004543ab2562dd948a29fd2530f143a7cf337a59"
    # ADSL-1-read_data -------------------------------------------------------------
    cnt <- connector::connect(config = "_connector.yml")
      DM <- cnt$sdtm$read_cnt(tolower('DM')) |>
      dplyr::select(AGE,ARM,STUDYID,USUBJID)

    # ADSL-init_domain -------------------------------------------------------------
    DM <- DM |>
      dplyr::mutate(SRC_ = "DM")

    ADSL <- DM |>
        dplyr::select(AGE,ARM,STUDYID,USUBJID,SRC_) |>
        admiral::convert_blanks_to_na()

    # ADSL-filter_domain -----------------------------------------------------------
    ADSL <- ADSL |>
      dplyr::filter((SRC_ == 'DM' & !is.na(AGE))) |>
      dplyr::select(-SRC_)

    ADSL <- ADSL |>
      dplyr::select(AGE,ARM,STUDYID,USUBJID)


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

### Save the programs

To save the programs as `.R` files, pass them to
[`write_adam_programs()`](https://novonordisk-opensource.github.io/mighty/reference/write_adam_programs.md).
The folder must already exist:

``` r

dir.create("programs")
write_adam_programs(results$programs, dir = "programs")
# Creates programs/1_ADSL.R
```

Use `style = TRUE` to format the files with styler. We recommend saving
the programs outside `study_dir/`, so that folder holds only
specifications (see [Study directory
guidelines](#study-directory-guidelines)).

### Next steps

- [`vignette("inputs_overview")`](https://novonordisk-opensource.github.io/mighty/articles/inputs_overview.md):
  all inputs mighty uses, in one place
- [`vignette("mighty_config")`](https://novonordisk-opensource.github.io/mighty/articles/mighty_config.md):
  keys and component repositories in `_mighty.yml`
- [`vignette("adam_specification")`](https://novonordisk-opensource.github.io/mighty/articles/adam_specification.md):
  everything a specification can contain: population, columns, rows,
  parameters
- [`vignette("code_components")`](https://novonordisk-opensource.github.io/mighty/articles/code_components.md):
  how to write your own components
- [`vignette("connect_to_data")`](https://novonordisk-opensource.github.io/mighty/articles/connect_to_data.md):
  how the generated programs read and write data
- [`vignette("missing_data_analysis")`](https://novonordisk-opensource.github.io/mighty/articles/missing_data_analysis.md):
  generating programs that run on the data available today
