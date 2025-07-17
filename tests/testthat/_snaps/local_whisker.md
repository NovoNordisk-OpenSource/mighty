# Depends parameters replaced with actual user-supplied values

    Code
      strsplit(actual$program_sequence[outputs == "A", code], "\n")[[1]][4:5]
    Output
      [1] "A <- A |> "                  "  dplyr::mutate(U2=USUBJID)"

