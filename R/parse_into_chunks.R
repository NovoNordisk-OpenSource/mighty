#' Parse into chunks
#'
#' @param code_id
#' @param node_id
#' @param domain_name
#' @param outputs
#' @param env
#'
#' @return
#' @export
#'
#' @examples
parse_into_chunks <- function(code_id,
                              user_supplied_parameters,
                              node_id,
                              domain_name,
                              outputs,
                              env) {

  # Get function definition from the environment
  f_def <- get(code_id, envir = env)

  # Check that the function follows the required standards
  validate_std_action_return(f_def = f_def, fn_name = code_id)

  # Convert function to code chunk. We have to do it this way, and not use
  # `f_src_ref` in order to preserve any comments. This makes it harded to
  # un-ambiguously find the header and final return statement for functions that
  # have embedded functions
  code_chunk <- attr(f_def, "srcref") |> paste(collapse = "\n") |>
    remove_function_header() |>
    remove_function_return()
  out <- gsub(".self", domain_name, code_chunk)

  # Temporary objects to remove
  interim_objects_to_rm <- get_scope_objects(f_def)

  # Add params assignment if it exists and update objects to remove
  params <- formals(f_def)[["params"]]
  params_exist <- !is.null(params)
  if (params_exist) {

    updated_params <- update_default_parameters(params, user_supplied_parameters)
    params_expr <- rlang::expr(params <- !!updated_params)
    params_code <- rlang::expr_text(params_expr)
    out <- glue::glue(
    '{params_code}
    {out}')
    interim_objects_to_rm <- c(interim_objects_to_rm, "params")
  }

  if(length(interim_objects_to_rm) > 0) {
    out <- glue::glue(
      '{out}
      # Remove interim objects
      rm({paste(interim_objects_to_rm, collapse = ", ")})
      \n'
    )
  }

  # Add header
  header <- node_id
  # header <- paste(sort(outputs[[1]]), collapse = ", ")
  out_glue <- glue::glue(
    '# {header} -----------------------------------------------------
    {out}\n'
  )

  return(out_glue)

}

remove_function_header <- function(f_string) {
  # grep_pattern <- "function\\(\\s*\\.self\\s*(,\\s*\\w+)*\\s*\\)\\s*\\{"
  grep_pattern <- "function\\(\\s*\\.self.*\\)\\s*?\\{"

  # When there are function definitions embedded in a node, we need to allow
  # those to remain
  gsub(pattern = grep_pattern,
       replacement = "",
       x = f_string)

}

remove_function_return <- function(f_string) {
  # TODO: check for when the return statement is broken up by linebreaks
  grep_pattern <- "return\\(.self\\)(?s:.*)\\}"
  gsub(
    pattern = grep_pattern,
    replacement = "",
    x = f_string,
    perl = TRUE
  )

}

validate_std_action_return <- function(f_def, fn_name) {
  # First arg is ".self
  args <- formalArgs(f_def)
  if (args[1] != ".self") {
    stop("Error in ",
         fn_name,
         ". First argument must be `.self`, but ",
         fn_name,
         " has: `",
         args[1],
         "`")
  }
  # Ends with "return(.self)"
  f_src_ref <- attr(body(f_def), "srcref")
  src_length <- length(f_src_ref)
  actual <- f_src_ref[[src_length]] |> as.character()
  expected <- "return(.self)"
  result <- identical(actual, expected)
  if (!result) {
    stop("Source function required to end with `",
         expected,
         "`",
         fn_name,
         " ends with: \n",
         actual)
  }
  return(TRUE)


}
