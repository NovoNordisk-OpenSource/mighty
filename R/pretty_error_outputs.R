pretty_error_outputs <- function(lst) {
  # Initialize the error message with the parent list element name
  error_message_intro_1 <- "    Expected column outputs:\n"
  error_message_intro_2 <- "    Actual column outputs:\n"
  missing_from_code <- "    Missing from expected outputs:\n"
  # Add each child element to the error message
  error_message <- vector("character")

  for (child in names(lst)) {
    expected <- lst[[child]][["Outputs_from_code_component"]]
    actual <- lst[[child]][["Outputs_from_specificaton"]]

    error_message <- c(
      error_message,
      paste0(
        "- ",
        child,
        ":\n ",
        error_message_intro_1,
        paste0("\"", expected, "\"", collapse = ",\n"),
        "\n",
        error_message_intro_2,
        paste0("\"", actual, "\"", collapse = ",\n"),
        "\n",
        paste0(missing_from_code, setdiff(expected, actual), collapse = "\n"),
        "\n-----------\n"
      )
    )
  }
  return(error_message)
}
