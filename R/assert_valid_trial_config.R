assert_valid_trial_config <- function(trial_metadata) {
  # Check if trial_metadata is a list
  if (!is.list(trial_metadata)) {
    stop("trial_metadata must be a list.")
  }

  # Check if required fields are present
  required_fields <- c("trial_id", "project_id", "complete_id", "instance")
  missing_fields <- setdiff(required_fields, names(trial_metadata))
  if (length(missing_fields) > 0) {
    stop(paste("Missing required fields in trial_metadata:", paste(missing_fields, collapse = ", ")))
  }

  # Check if fields are non-empty
  for (field in required_fields) {
    if (is.null(trial_metadata[[field]]) || !is.character(trial_metadata[[field]]) || trial_metadata[[field]] == "") {
      stop(paste("Missing value for required fields in trial_metadata:", field, "cannot be NULL and must be a non-empty string."))
    }
  }
}
