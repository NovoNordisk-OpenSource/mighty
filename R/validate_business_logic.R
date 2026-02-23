#' Validate business logic rules for YAML configuration
#'
#' @param yaml_content Raw YAML content structure from mighty_metadata
#' @param domain_name Path to YAML file (for error context)
#' @param ruleset_name Schema name to determine applicable rules
#' @return Invisible NULL if valid, throws error if invalid
#' @noRd
validate_business_logic <- function(
  yaml_content,
  domain_name,
  ruleset_name = "adam_domain"
) {
  # Get applicable rules for this schema
  rules <- get_business_rules(ruleset_name)

  if (length(rules$stop_on_error) == 0 && length(rules$collect_errors) == 0) {
    return(invisible(NULL))
  }

  # Create context for rules
  context <- list(
    domain_name = domain_name,
    ruleset_name = ruleset_name,
    basename = basename(domain_name)
  )

  # Run stop-on-error rules first - halt execution on first failure
  if (length(rules$stop_on_error) > 0) {
    for (rule_name in names(rules$stop_on_error)) {
      rule_func <- rules$stop_on_error[[rule_name]]

      result <- tryCatch(
        {
          rule_func(yaml_content, context)
        },
        error = function(e) {
          # If rule itself fails, that's a bug in the rule
          cli::cli_warn(c(
            "!" = "Stop-on-error business rule '{rule_name}' failed to execute",
            "x" = "Error: {e$message}"
          ))
          return(list(valid = TRUE, errors = character(0)))
        }
      )

      if (!result$valid && length(result$errors) > 0) {
        error_messages <- result$errors
        names(error_messages) <- rep("*", length(error_messages))

        cli::cli_abort(
          c(
            "x" = "Validation failed for {domain_name} with the following error(s):",
            error_messages
          ),
          class = "validation_error"
        )
      }
    }
  }

  # Run collect-errors rules and accumulate all errors
  all_errors <- list()

  for (rule_name in names(rules$collect_errors)) {
    rule_func <- rules$collect_errors[[rule_name]]

    tryCatch(
      {
        result <- rule_func(yaml_content, context)

        if (!result$valid && length(result$errors) > 0) {
          all_errors[[rule_name]] <- result$errors
        }
      },
      error = function(e) {
        # If rule itself fails, that's a bug in the rule
        cli::cli_warn(c(
          "!" = "Collect-errors business rule '{rule_name}' failed to execute",
          "x" = "Error: {e$message}"
        ))
      }
    )
  }

  # Handle any collected errors from collect-errors rules
  if (length(all_errors) > 0) {
    handle_business_logic_errors(all_errors, domain_name)
  }

  return(invisible(yaml_content))
}

#' Get business rules for a given schema
#'
#' @param ruleset_name Name of schema to get rules for
#' @return Named list with 'stop_on_error' and 'collect_errors' rule functions
#' @noRd
get_business_rules <- function(ruleset_name) {
  # Registry of rules by schema
  rule_registry <- list(
    adam_domain = list(
      stop_on_error = register_rules(
        val_method_and_component_id_not_both_populated
      ),
      collect_errors = register_rules(
        val_no_duplicate_columns,
        val_no_duplicate_row_parameter_ids,
        val_depend_rows,
        val_keys_included_as_columns
      )
    )
  )
  rule_registry[[ruleset_name]] %||%
    list(stop_on_error = list(), collect_errors = list())
}

# Update your handle_business_logic_errors function
handle_business_logic_errors <- function(all_errors, domain_name) {
  error_messages <- unlist(all_errors, use.names = FALSE)
  names(error_messages) <- rep("*", length(error_messages))

  throw_yaml_validation_error(domain_name, error_messages)
}

# Auto-names the rules based on their function name so dev doesn't have to manually type out each name
register_rules <- function(...) {
  fns <- list(...)
  calls <- as.list(match.call(expand.dots = FALSE)$...)
  nms <- vapply(
    calls,
    function(x) paste(deparse(x), collapse = " "),
    character(1)
  )
  names(fns) <- nms
  fns
}
