# Write ADaM Programs to Disk

Writes generated ADaM programs to individual R files and optionally
applies code styling.

## Usage

``` r
write_adam_programs(programs, dir, style = FALSE)
```

## Arguments

- programs:

  Named list. Each element contains program code as character vector,
  with names used as file names (without .R extension).

- dir:

  Character. Output directory path where R files will be written.

- style:

  Logical. Whether to apply code styling using
  [`styler::style_file()`](https://styler.r-lib.org/reference/style_file.html).
  Default is FALSE.

## Value

Logical. Returns TRUE invisibly upon successful completion.

## Details

For each program in the list:

- Creates filename by appending ".R" to the list element name

- Collapses code vector into single string with newline separators

- Writes code to file in specified directory

- Optionally applies styler formatting if requested

## Examples

``` r
if (FALSE) { # \dontrun{
# Write programs without styling
programs <-  list(
  ADSL = c("library(dplyr)", "ADSL <- DM |> select(USUBJID)"),
  ADAE = c("library(dplyr)", "ADAE <- AE |> filter(!is.na(AEDECOD))")
)

write_adam_programs(programs, dir = "output/adam")
# Creates: output/adam/ADSL.R, output/adam/ADAE.R

# Write with styling
write_adam_programs(programs, dir = "output/adam", style = TRUE)
} # }
```
