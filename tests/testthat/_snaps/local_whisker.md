# Depends parameters replaced with actual user-supplied values

    Code
      strsplit(actual$program_sequence[outputs == "A", code], "\n")[[1]][3:4]
    Output
      [1] "A <- A |>"                   "  dplyr::mutate(U2=USUBJID)"

