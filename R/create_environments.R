create_one_environment <- function(i, environments, expt_def) {
  set.seed(environments$temperature_seed[i])

  temperature_series <- tibble::tibble(
    phase = c(
      rep("burn_in", expt_def$burn_in_duration),
      rep("expt", expt_def$experiment_duration + 1)
    ),
    time = 0:(expt_def$burn_in_duration + expt_def$experiment_duration),
    temperature = c(
      rep(expt_def$temperature_mean, expt_def$burn_in_duration),
      as.numeric(
        scale(
          primer::one_over_f(
            gamma = environments$one_over_f_gamma[i],
            N = expt_def$experiment_duration + 1
          )
        )
      ) * expt_def$temperature_sd + expt_def$temperature_mean
    ),
    env_series_id = environments$env_series_id[i]
  ) |>
    dplyr::mutate(temperature = ifelse(
      phase == "burn_in",
      temperature[expt_def$burn_in_duration + 1],
      temperature
    ))

  temperature_series |>
    dplyr::filter(time > expt_def$burn_in_duration)
}

#' Legacy JSON temperature time-series creator
#'
#' This internal helper creates temperature time series for the old JSON
#' experiment format. New user-facing workflows should use
#' [create_environments_from_spec()] through [run_experiment()].
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing temperatures database.
#' @param verbose Logical. If `TRUE`, print messages about written outputs.
#'
#' @details Old experiment JSON files can optionally include
#'   `parallel_environments`, `parallel_simulations`, `parallel_workers`,
#'   `environment_progress`, and `runtime_update_every`. When
#'   `parallel_environments` evaluates to `TRUE`, or when it is absent and
#'   `parallel_simulations` evaluates to `TRUE`, environmental time series are
#'   generated in parallel and SQLite tables are still written serially by the
#'   parent process. Parallel processing is intended for macOS/Linux.
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series.
#' @keywords internal
#'
#' @examples NULL
create_environments <- function(experiment_folder,
                                experiment_design_filename,
                                overwrite = FALSE,
                                verbose = TRUE) {

  require_dbplyr()

  ## setup the databases for saving the temperature time series
  output_path <- paste0(experiment_folder, "temperatures.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "temperatures database")
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  on.exit({
    if (DBI::dbIsValid(conn_temperatures)) {
      DBI::dbDisconnect(conn_temperatures)
    }
  }, add = TRUE)

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

  environments <- expt |>
    dplyr::select(env_series_id, temperature_mean, temperature_sd, one_over_f_gamma, temperature_seed) |>
    dplyr::distinct()

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  get_design_value <- function(name, default) {
    value <- expt_def[[name]]
    if (is.null(value) || length(value) == 0) {
      return(default)
    }
    value <- value[[1]]
    if (is.character(value)) {
      parsed_value <- try(eval(parse(text = value)), silent = TRUE)
      if (!inherits(parsed_value, "try-error")) {
        return(parsed_value)
      }
    }
    value
  }

  parallel_environments <- isTRUE(get_design_value(
    "parallel_environments",
    get_design_value("parallel_simulations", FALSE)
  ))
  parallel_workers <- as.integer(get_design_value(
    "parallel_workers",
    max(1, parallel::detectCores(logical = FALSE) - 1)
  ))
  if (is.na(parallel_workers)) {
    parallel_workers <- 1
  }
  parallel_workers <- max(1, min(parallel_workers, nrow(environments)))
  runtime_update_every <- as.integer(get_design_value(
    "runtime_update_every",
    max(1L, floor(nrow(environments) / 100))
  ))
  if (is.na(runtime_update_every) || runtime_update_every < 1) {
    stop("`runtime_update_every` must evaluate to an integer >= 1.", call. = FALSE)
  }
  environment_progress <- isTRUE(get_design_value("environment_progress", verbose))

  if (parallel_environments && parallel_workers > 1 && .Platform$OS.type == "windows") {
    warning(
      "`parallel_environments = TRUE` uses forked parallel workers, which are ",
      "not available on Windows. Falling back to serial environment generation.",
      call. = FALSE
    )
    parallel_environments <- FALSE
  }

  environment_indices <- seq_len(nrow(environments))
  environment_table_written <- FALSE
  completed_environments <- 0L
  environment_start_time <- Sys.time()
  last_runtime_update_completed <- 0L
  progress_bar <- NULL

  if (environment_progress) {
    progress_bar <- utils::txtProgressBar(
      min = 0,
      max = length(environment_indices),
      initial = 0,
      style = 3
    )
    on.exit({
      if (!is.null(progress_bar)) {
        close(progress_bar)
      }
    }, add = TRUE)
  }

  update_progress <- function(n = 1L) {
    completed_environments <<- completed_environments + n
    if (!is.null(progress_bar)) {
      utils::setTxtProgressBar(progress_bar, completed_environments)
    }
  }

  write_environment_result <- function(temperature_series) {
    DBI::dbWriteTable(
      conn_temperatures,
      "temperatures",
      temperature_series,
      overwrite = !environment_table_written,
      append = environment_table_written
    )
    environment_table_written <<- TRUE
  }

  report_runtime_estimate <- function(force = FALSE) {
    if (!verbose || completed_environments == 0L) {
      return(invisible(FALSE))
    }
    if (force && last_runtime_update_completed == completed_environments) {
      return(invisible(FALSE))
    }
    if (!force &&
        completed_environments - last_runtime_update_completed < runtime_update_every &&
        completed_environments < length(environment_indices)) {
      return(invisible(FALSE))
    }

    elapsed_seconds <- as.numeric(difftime(Sys.time(), environment_start_time, units = "secs"))
    seconds_per_environment <- elapsed_seconds / completed_environments
    remaining_environments <- length(environment_indices) - completed_environments
    remaining_seconds <- seconds_per_environment * remaining_environments

    if (!is.null(progress_bar)) {
      cat("\n")
    }
    message(
      "Environment generation runtime: ",
      completed_environments,
      "/",
      length(environment_indices),
      " series complete; elapsed ",
      format_simulation_duration(elapsed_seconds),
      "; estimated remaining ",
      format_simulation_duration(remaining_seconds)
    )
    last_runtime_update_completed <<- completed_environments
    invisible(TRUE)
  }

  record_environment_completion <- function(temperature_series) {
    write_environment_result(temperature_series)
    update_progress()
    report_runtime_estimate()
  }

  generate_environment <- function(i) {
    create_one_environment(i, environments, expt_def)
  }

  if (parallel_environments && parallel_workers > 1) {
    if (verbose) {
      message("Creating environments in parallel with ", parallel_workers, " workers")
    }

    pending_environments <- environment_indices
    active_jobs <- list()

    start_job <- function(environment_index) {
      job <- parallel::mcparallel(
        generate_environment(environment_index),
        name = as.character(environment_index),
        silent = TRUE
      )
      list(environment_index = environment_index, job = job)
    }

    while (length(pending_environments) > 0 || length(active_jobs) > 0) {
      while (length(pending_environments) > 0 && length(active_jobs) < parallel_workers) {
        next_environment <- pending_environments[[1]]
        pending_environments <- pending_environments[-1]
        active_jobs[[as.character(next_environment)]] <- start_job(next_environment)
      }

      job_list <- lapply(active_jobs, `[[`, "job")
      collected <- parallel::mccollect(job_list, wait = FALSE)

      if (length(collected) == 0) {
        Sys.sleep(0.1)
        next
      }

      for (pid in names(collected)) {
        if (pid %in% names(active_jobs)) {
          active_name <- pid
        } else {
          active_match <- vapply(
            active_jobs,
            function(active_job) as.character(active_job$job$pid) == pid,
            logical(1)
          )
          if (!any(active_match)) {
            next
          }
          active_name <- names(active_jobs)[active_match][[1]]
        }
        environment_index <- active_jobs[[active_name]]$environment_index
        environment_result <- collected[[pid]]
        if (inherits(environment_result, "try-error")) {
          stop(
            "Environment series ", environment_index, " failed in a parallel worker: ",
            conditionMessage(attr(environment_result, "condition")),
            call. = FALSE
          )
        }
        record_environment_completion(environment_result)
        active_jobs[[active_name]] <- NULL
      }
    }
  } else {
    for(i in environment_indices) {
      temperature_series_expt_only <- generate_environment(i)
      record_environment_completion(temperature_series_expt_only)
    }
  }

  if (!is.null(progress_bar)) {
    close(progress_bar)
    progress_bar <- NULL
  }

  report_runtime_estimate(force = TRUE)
  DBI::dbDisconnect(conn_temperatures)
  announce_output_written(output_path, verbose = verbose, label = "temperatures database")

}
