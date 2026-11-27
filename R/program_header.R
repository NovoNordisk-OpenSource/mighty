#' Generate the Header for a Generated Program
#'
#' @description
#' Builds the comment block placed at the top of every generated program. It
#' lists the dataset the program creates, the datasets it reads, the
#' programmer, when the programs were generated and with which mighty version.
#'
#' @param program_actions Data table with the actions of one program,
#'   including its `read_data` action.
#' @param part Integer. Position of this program among the programs of its
#'   domain.
#' @param n_parts Integer. Number of programs for the domain.
#' @param generated_at POSIXct. Time the generation was started. The same
#'   value is used for every program of one generation run.
#' @param programmer Character. Programmer of the domain, see
#'   [find_programmer()].
#'
#' @return Character string with the header, without a trailing newline.
#' @noRd
gen_program_header <- function(
  program_actions,
  part,
  n_parts,
  generated_at,
  programmer
) {
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
    format_header_field("Programmer", programmer),
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

#' Find the Programmer of a Domain
#'
#' @description
#' Uses the author of the last git commit that changed the domain's
#' specification file. Reads the repository with the gert package, so the git
#' command line tool is not needed.
#'
#' Returns `"Not specified"` when the author cannot be found reliably:
#' \itemize{
#'   \item gert is not installed
#'   \item the domain has no file, or the file is not in a git repository
#'   \item the file has no commits, or has changes that are not committed
#'   \item the repository is a shallow clone, where the last commit can show
#'     every file as changed
#' }
#'
#' @param file Character or `NULL`. Path to the domain specification file.
#'
#' @return Character string with the author's name.
#' @noRd
find_programmer <- function(file) {
  not_specified <- "Not specified"
  if (is.null(file) || length(file) != 1 || !file.exists(file)) {
    return(not_specified)
  }
  if (!gert_available()) {
    return(not_specified)
  }
  tryCatch(
    {
      repo <- gert::git_find(dirname(file))
      if (file.exists(file.path(repo, ".git", "shallow"))) {
        return(not_specified)
      }
      rel_file <- relative_path(file, repo)
      if (nrow(gert::git_status(pathspec = rel_file, repo = repo)) > 0) {
        return(not_specified)
      }
      head <- gert::git_stat_files(rel_file, repo = repo)$head
      if (length(head) != 1 || is.na(head)) {
        return(not_specified)
      }
      author <- gert::git_commit_info(head, repo = repo)$author
      # Drop the e-mail address from "Name <email>"
      sub("\\s*<[^>]*>$", "", author)
    },
    error = function(e) not_specified
  )
}

relative_path <- function(file, root) {
  file <- normalizePath(file, winslash = "/")
  root <- normalizePath(root, winslash = "/")
  substring(file, nchar(root) + 2)
}

# Wrapper so tests can simulate gert not being installed
gert_available <- function() {
  requireNamespace("gert", quietly = TRUE)
}
