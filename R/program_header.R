#' Generate the Header for a Generated Program
#'
#' @description
#' Builds the comment block placed at the top of every generated program. It
#' lists the dataset the program creates, the datasets it reads, when the
#' programs were generated and with which mighty version.
#'
#' @param program_actions Data table with the actions of one program,
#'   including its `read_data` action.
#' @param part Integer. Position of this program among the programs of its
#'   domain.
#' @param n_parts Integer. Number of programs for the domain.
#' @param generated_at POSIXct. Time the generation was started. The same
#'   value is used for every program of one generation run.
#'
#' @return Character string with the header, without a trailing newline.
#' @noRd
gen_program_header <- function(program_actions, part, n_parts, generated_at) {
  domain <- program_actions$domain[[1]]
  description <- paste0("Creates the ", domain, " analysis dataset")
  if (n_parts > 1) {
    description <- paste0(description, " (part ", part, " of ", n_parts, ")")
  }
  description <- paste0(description, ".")

  banner <- paste0("# ", strrep("*", 78))
  fields <- c(
    format_header_field("Description", description),
    format_header_field("Input", format_program_inputs(program_actions)),
    format_header_field("Output", domain),
    format_header_field(
      "Usage notes",
      c(
        "Generated from the Mighty study specification.",
        "Modify the specification or source components and regenerate;",
        "do not modify this generated program directly."
      )
    ),
    format_header_field("Generated", format_generation_time(generated_at)),
    format_header_field("Generator", paste0("mighty v", mighty_version()))
  )
  paste(c(banner, fields, banner), collapse = "\n")
}

#' List the Datasets a Program Reads
#'
#' @description
#' Takes the datasets from the program's `read_data` action. Datasets whose
#' columns are all missing from the available data are kept and marked as
#' `(not available)`.
#'
#' @param program_actions Data table with the actions of one program.
#'
#' @return Character string with the comma separated, sorted dataset names,
#'   or `"None"` when the program reads no datasets.
#' @noRd
format_program_inputs <- function(program_actions) {
  read_action <- program_actions[
    program_actions$code_id == "mighty_read_data",
  ]
  available <- column_ref_domains(unlist(read_action$outputs))
  removed <- character(0)
  if ("removed_outputs" %in% names(read_action)) {
    removed <- column_ref_domains(unlist(read_action$removed_outputs))
  }
  not_available <- setdiff(removed, available)
  inputs <- sort(c(available, not_available))
  if (length(inputs) == 0) {
    return("None")
  }
  inputs[inputs %in% not_available] <- paste(
    inputs[inputs %in% not_available],
    "(not available)"
  )
  paste(inputs, collapse = ", ")
}

column_ref_domains <- function(column_refs) {
  column_refs <- column_refs[!is.na(column_refs)]
  unique(sub("\\..*$", "", column_refs))
}

#' Format One Header Field
#'
#' Wraps each element of `value` at word boundaries so no line is longer than
#' 80 characters, indenting continuation lines to the value column. Each
#' element of `value` starts on a new line.
#' @noRd
format_header_field <- function(label, value) {
  prefix <- formatC(paste0(label, ":"), width = -15)
  lines <- unlist(lapply(
    value,
    strwrap,
    width = 80 - nchar("# ") - nchar(prefix)
  ))
  lines[1] <- paste0(prefix, lines[1])
  lines[-1] <- paste0(strrep(" ", nchar(prefix)), lines[-1])
  paste0("# ", lines)
}

format_generation_time <- function(generated_at) {
  format(generated_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

# Wrappers so tests can fix the values shown in program headers
generation_time <- function() {
  Sys.time()
}

mighty_version <- function() {
  as.character(utils::packageVersion("mighty"))
}
