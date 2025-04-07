parse_node_metadata <- function(file_path) {

  lines <- readLines(file_path, warn = FALSE) |> trimws()
  out <- list()
  current_function <- NULL
  current_metadata <- NULL
  in_metadata <- FALSE
  functions <- get_top_level_functions(file_path)
  metadata_lines <- metadata_block_lines(lines)
  for (i in seq_along(lines)) {
    line <- lines[i]
    in_metadata <- i %in% metadata_lines
    in_function_line <- i %in% functions$line
    if (!in_metadata && !in_function_line) {
      next
    }

    if (in_function_line) {
      # This always comes after thes metadata chunk
      current_function <- functions[i == line, closure_name]
      out[[current_function]] <- yaml::yaml.load(current_metadata)
      current_metadata <- NULL
      current_function <- NULL
      next
    }

    current_metadata <- paste0(current_metadata, "\n", stringr::str_remove(line, "^# "))
  }

  # # Handle case where the file ends while still in metadata block
  # if (!is.null(current_function) && !is.null(current_metadata)) {
  #   current_function$metadata <- yaml::yaml.load(current_metadata)
  #   out[[current_function$name]] <- current_function
  # }

  return(out)
}


metadata_block_lines <- function(content) {
  in_metadata <- FALSE
  start_marker <- FALSE
  out <- list()
  for (i in seq_along(content)) {
    line <- content[i]
    marker <- startsWith(line, "#!-!")
    if (!in_metadata && marker) {
      in_metadata <- TRUE

    } else if (in_metadata && marker) {
      in_metadata <- FALSE

    } else if (in_metadata && !marker) {
      out[[paste0(i)]] <- i
    }
  }

  out  |> unname() |> unlist()
}

get_top_level_functions <- function(file_path) {
  # Parse the code using base R
  parsed <- parse(file_path, keep.source = TRUE)
  srcref_data <- attr(parsed, "srcref")

  # Function to recursively find function assignments
  find_function_assignments <- function(expr, srcref_i) {
    if (!is.call(expr)) {
      return(NULL)
    }
    if (!is_function_definition(expr))
      return(NULL)
    out <- list(name = as.character(expr[[2]]), line = srcref_i[1])
    return(out)
  }
  is_function_definition <- function(expr) {
    as.character(expr[[1]]) %in% c("<-", "=") &&
      is.call(expr[[3]]) &&
      as.character(expr[[3]][[1]]) == "function"
  }


  # Find all function assignments

  all_functions <- purrr::map2(as.list(parsed), srcref_data, find_function_assignments) |>
    data.table::rbindlist() |>
    data.table::setnames(c("closure_name", "line"))

  return(all_functions)
}

combine_metadata_config <- function(functions, config, file_path) {
  combined <- list()

  for (func_name in names(functions)) {

    func <- functions[[func_name]]
    func_config <- config[[func_name]]

    if (is.null(func_config)) {
      next
    }
    depends_and_output <- add_ADaM_domain(func, func_config$self_domain)
    combined_entry <- list(
      type = func$type,
      depends = depends_and_output$depends,
      depends_row = func_config$depends_row,
      modifies = depends_and_output$output
    )

    # Store output
    combined[[paste0(func_config$self_domain, ".", func_name)]] <- combined_entry
  }
  yaml::write_yaml(combined, file_path)
  return(combined)
}

add_ADaM_domain <- function(metadata_list, domain) {
  deps <- metadata_list$depends
  output <- metadata_list$output

  list(
    depends = gsub("self\\.", paste0(domain, "."), deps),
    output = gsub("self\\.", paste0(domain, "."), output)
  )

}
