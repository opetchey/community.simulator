# Process management must not consume the scientific RNG stream in the parent.
with_preserved_worker_rng <- function(expr) {
  seed <- get0(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  on.exit({
    if (is.null(seed)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
        rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", seed, envir = .GlobalEnv)
  }, add = TRUE)
  force(expr)
}

# Independent exec'd R sessions: no R work runs in a forked parent image.
supports_parallel <- function(enabled, workers) isTRUE(enabled) && workers > 1L

read_temperature_data <- function(path) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  dplyr::collect(dplyr::tbl(conn, "temperatures"))
}

write_stage_table <- function(path, table, value, append) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  DBI::dbWriteTable(conn, table, value, overwrite = !append, append = append)
  invisible(NULL)
}

validate_case_rows <- function(value, expected, label, ordered = TRUE) {
  actual <- value$case_id
  if (!is.data.frame(value) || is.null(actual) || nrow(value) != length(expected) ||
      anyNA(actual) || anyDuplicated(actual) || anyDuplicated(expected) ||
      !setequal(as.character(actual), as.character(expected)) ||
      (ordered && !identical(as.character(actual), as.character(expected)))) {
    stop(label, " returned missing, duplicate, unexpected or out-of-order case IDs; expected: ",
         paste(expected, collapse = ", "), call. = FALSE)
  }
  invisible(value)
}

# Only this function and explicitly constructed payloads cross the process boundary.
# It is a namespace function, not a closure over the stage's data/connections.
execute_stage_chunk <- function(payload) {
  if (!is.null(payload$rng_kind)) do.call(RNGkind, as.list(payload$rng_kind))
  switch(payload$stage,
    create_environments = lapply(seq_len(nrow(payload$environments)), function(i) {
      value <- create_prepared_environment(payload$environments, payload$settings, i)
      attr(value, "worker_rng_state") <- get(".Random.seed", envir = .GlobalEnv)
      value
    }),
    simulate_dynamics = lapply(seq_len(nrow(payload$expt)), function(i) {
      simulate_prepared_case(payload$expt, payload$communities, payload$temperatures,
                             payload$settings, i)
    }),
    get_community_measures = {
      result <- get_community_CPC_measures(payload$temperatures, payload$expt,
        list(), every_t = 1, soft_viability_scale = payload$soft_viability_scale,
        verbose = FALSE)
      lapply(seq_len(nrow(result)), function(i) result[i, , drop = FALSE])
    },
    stop("Unknown parallel stage: ", payload$stage)
  )
}

# Callbacks build payloads and commit results only in the parent. The bounded
# dispatch window prevents a slow early chunk accumulating the whole run in RAM.
run_supervised_chunks <- function(indices, workers, make_payload, on_result,
                                   progress_reporter, step, case_ids,
                                   timeout_minutes = 120, retries = 2L,
                                   chunk_size = 10L, log_path = NULL,
                                   worker = execute_stage_chunk) {
  require_positive_number(timeout_minutes, "parallel.worker_timeout_minutes")
  require_nonnegative_integer(retries, "parallel.worker_retries")
  require_positive_integer(chunk_size, "parallel.chunk_size")
  timeout_minutes <- as.numeric(timeout_minutes)
  retries <- as.numeric(retries)
  chunk_size <- as.numeric(chunk_size)
  if (!length(indices)) return(invisible(NULL))
  chunks <- split(indices, ceiling(seq_along(indices) / chunk_size))
  workers <- min(as.integer(workers), length(chunks))
  jobs <- vector("list", length(chunks))
  results <- vector("list", length(chunks))
  attempts <- integer(length(chunks))
  finished <- logical(length(chunks))
  workdir <- tempfile("community-workers-")
  dir.create(workdir)
  terminate <- function(process) {
    if (is.null(process)) return(invisible(NULL))
    with_preserved_worker_rng({
      try(process$kill_tree(), silent = TRUE)
      try(process$kill(), silent = TRUE)
      try(process$wait(timeout = 1000), silent = TRUE)
    })
    invisible(NULL)
  }
  on.exit(suspendInterrupts({
    for (job in jobs) if (!is.null(job)) terminate(job$process)
    unlink(workdir, recursive = TRUE)
  }), add = TRUE)
  log_event <- function(event, k, elapsed, reason = NULL, pid = NULL) {
    if (!is.null(log_path)) append_experiment_log_entry(log_path, event,
      list(step = step, chunk_id = k, case_ids = unname(case_ids[chunks[[k]]]),
           attempt = attempts[k], elapsed_seconds = elapsed, reason = reason, pid = pid))
  }
  fail <- function(k, event, reason, elapsed) {
    job <- jobs[[k]]
    pid <- if (!is.null(job$process)) job$process$get_pid() else NULL
    terminate(job$process)
    jobs[k] <<- list(NULL)
    log_event(event, k, elapsed, reason, pid)
    if (attempts[k] > retries) {
      stop(step, " chunk ", k, " failed after ", attempts[k], " attempt(s); cases: ",
           paste(case_ids[chunks[[k]]], collapse = ", "), ". ", reason, call. = FALSE)
    }
    log_event("worker_retry", k, elapsed, reason, pid)
  }
  next_commit <- 1L
  completed <- 0L
  while (next_commit <= length(chunks)) {
    window <- seq.int(next_commit, min(length(chunks), next_commit + workers - 1L))
    for (k in window) {
      if (finished[k] || !is.null(jobs[[k]])) next
      attempts[k] <- attempts[k] + 1L
      started <- proc.time()[["elapsed"]]
      payload <- make_payload(chunks[[k]])
      payload$rng_kind <- RNGkind()
      # Register the handle before delivering an interrupt, so cleanup can
      # always reach a process created during startup.
      suspendInterrupts({
        launched <- tryCatch(with_preserved_worker_rng(callr::r_bg(
          func = worker, args = list(payload),
          libpath = .libPaths(), supervise = TRUE, package = "community.simulator",
          stdout = file.path(workdir, paste0(k, ".out")),
          stderr = file.path(workdir, paste0(k, ".err")),
          system_profile = FALSE, user_profile = FALSE)), error = identity)
        if (inherits(launched, "error")) {
          jobs[[k]] <- list(process = NULL)
          fail(k, "worker_error", conditionMessage(launched), proc.time()[["elapsed"]] - started)
        } else {
          jobs[[k]] <- list(process = launched, started = started)
          log_event("worker_started", k, 0, pid = launched$get_pid())
        }
      })
    }
    for (k in window) {
      job <- jobs[[k]]
      if (is.null(job)) next
      elapsed <- proc.time()[["elapsed"]] - job$started
      if (job$process$is_alive()) {
        if (elapsed >= timeout_minutes * 60) {
          fail(k, "worker_timeout", paste0("Worker exceeded ", timeout_minutes,
               " minute chunk deadline."), elapsed)
        }
        next
      }
      value <- tryCatch(job$process$get_result(), error = identity)
      if (inherits(value, "error")) {
        event <- if (inherits(value$parent, "error")) "worker_error" else "worker_death"
        fail(k, event, conditionMessage(value), elapsed)
        next
      }
      if (!is.list(value) || length(value) != length(chunks[[k]]) ||
          any(vapply(value, is.null, logical(1)))) {
        fail(k, "worker_error", "Worker returned incomplete chunk results.", elapsed)
        next
      }
      results[[k]] <- value
      finished[k] <- TRUE
      terminate(job$process)
      jobs[k] <- list(NULL)
    }
    while (next_commit <= length(chunks) && finished[next_commit]) {
      for (j in seq_along(chunks[[next_commit]])) {
        on_result(chunks[[next_commit]][j], results[[next_commit]][[j]])
        completed <- completed + 1L
        progress_reporter(completed)
      }
      results[next_commit] <- list(NULL)
      next_commit <- next_commit + 1L
    }
    if (next_commit <= length(chunks)) Sys.sleep(0.05)
  }
  invisible(NULL)
}
