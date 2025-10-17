derivation_node <- R6::R6Class(
  "derivation_node",
  public = list(
    variable_name = NULL,
    parents = list(),
    children = list(),
    action = NULL,
    execution_status = "pending",

    initialize = function(variable_name, action) {
      self$variable_name <- variable_name
      self$action <- action
    },

    add_parent = function(parent_node) {
      self$parents <- append(self$parents, parent_node)
      parent_node$add_child(self)
    },

    add_child = function(child_node) {
      self$children <- append(self$children, child_node)
    },

    can_execute = function(data_context) {
      # Check if all parent dependencies are satisfied
      parents_ready <- all(sapply(self$parents, function(p) {
        p$execution_status == "completed"
      }))

      # Check if required data is available
      data_available <- self$action$can_execute_with_data(
        data_context$get_available_variables()
      )

      return(parents_ready && data_available)
    },

    execute = function(data_context) {
      if (self$can_execute(data_context)) {
        tryCatch(
          {
            result <- self$action$execute(data_context)
            self$execution_status <- "completed"
            return(result)
          },
          error = function(e) {
            self$execution_status <- "failed"
            stop(paste("Failed to execute", self$variable_name, ":", e$message))
          }
        )
      } else {
        self$execution_status <- "skipped"
        return(self$create_placeholder(data_context))
      }
    },

    create_placeholder = function(data_context) {
      # Create placeholder/default value when can't execute
      data <- data_context$get_data()
      data[, (self$variable_name) := NA]
      return(data)
    }
  )
)
