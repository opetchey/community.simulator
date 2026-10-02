format_ode_solver_output <- function(solver_output) {
  solver_output <- solver_output[nzchar(trimws(solver_output))]
  if (length(solver_output) == 0L) {
    return("")
  }

  paste0(
    "\nODE solver output:\n",
    paste(solver_output, collapse = "\n")
  )
}

run_ode_with_captured_output <- function(ode_args, failure_prefix) {
  solver_output <- character()
  solver_messages <- character()
  output_connection <- textConnection("solver_output", "w", local = TRUE)
  message_connection <- textConnection("solver_messages", "w", local = TRUE)
  output_sink_count <- sink.number()
  message_sink_count <- sink.number(type = "message")

  cleanup <- function() {
    while (sink.number(type = "message") > message_sink_count) {
      sink(type = "message")
    }
    while (sink.number() > output_sink_count) {
      sink()
    }
    close(output_connection)
    close(message_connection)
  }
  on.exit(cleanup(), add = TRUE)

  sink(output_connection)
  sink(message_connection, type = "message")

  error <- NULL
  value <- tryCatch(
    do.call(deSolve::ode, ode_args),
    error = function(e) {
      error <<- e
      NULL
    }
  )

  cleanup()
  on.exit(NULL, add = FALSE)

  captured <- c(solver_output, solver_messages)
  if (!is.null(error)) {
    stop(
      failure_prefix,
      conditionMessage(error),
      format_ode_solver_output(captured),
      call. = FALSE
    )
  }

  if (length(solver_output) > 0L) {
    cat(paste(solver_output, collapse = "\n"), "\n", sep = "")
  }
  if (length(solver_messages) > 0L) {
    message(paste(solver_messages, collapse = "\n"))
  }

  attr(value, "community.simulator.ode_solver_output") <- captured
  value
}
