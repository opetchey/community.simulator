format_bytes <- function(bytes) {
  units <- c("B", "KB", "MB", "GB", "TB")
  value <- as.numeric(bytes)
  unit_index <- 1L
  while (value >= 1024 && unit_index < length(units)) {
    value <- value / 1024
    unit_index <- unit_index + 1L
  }
  paste0(format(round(value, 1), trim = TRUE), " ", units[[unit_index]])
}

format_duration <- function(seconds) {
  seconds <- as.numeric(seconds)
  if (!is.finite(seconds) || seconds < 0) {
    return("unknown")
  }
  if (seconds < 60) {
    return(paste0(round(seconds), " sec"))
  }
  if (seconds < 3600) {
    return(paste0(round(seconds / 60, 1), " min"))
  }
  paste0(round(seconds / 3600, 1), " hr")
}

runtime_estimator_rates <- function(dynamics_type) {
  switch(
    dynamics_type,
    discrete = list(
      simulation_seconds_per_case_step = 1.877731e-05,
      community_measure_seconds_per_case = 0.01863,
      experiment_table_seconds_per_case = 0.00280,
      experiment_table_fixed_seconds = 0.25
    ),
    continuous = list(
      simulation_seconds_per_case_step = 5.0e-05,
      community_measure_seconds_per_case = 0.01863,
      experiment_table_seconds_per_case = 0.00280,
      experiment_table_fixed_seconds = 0.25
    ),
    consumer_resource_continuous = list(
      simulation_seconds_per_case_step = 4.295812e-05,
      community_measure_seconds_per_case = 0.13278,
      experiment_table_seconds_per_case = 0.00710,
      experiment_table_fixed_seconds = 0.05
    ),
    list(
      simulation_seconds_per_case_step = 5.0e-05,
      community_measure_seconds_per_case = 0.05,
      experiment_table_seconds_per_case = 0.003,
      experiment_table_fixed_seconds = 0.25
    )
  )
}

confirm_experiment_run <- function(summary) {
  message("")
  message("Experiment preflight summary")
  message("----------------------------")
  message("Cases: ", summary$n_cases)
  message("Dynamics type: ", summary$dynamics_type)
  message("Integration time points per case: ", summary$integration_steps)
  message("Saved dynamics/resource time points per case: ", summary$saved_time_points)
  if (summary$save_resources) {
    message("Saved resource time points per case: ", summary$saved_resource_time_points)
  }
  message("save_dynamics: ", summary$save_dynamics)
  message("save_resources: ", summary$save_resources)
  message("dynamics_save_every: ", summary$dynamics_save_every)
  if (summary$save_resources) {
    message("resources_save_every: ", summary$resources_save_every)
  }
  message("summary_checkpoint_every: ", summary$summary_checkpoint_every)
  message("runtime_update_every: ", summary$runtime_update_every)
  if (summary$save_dynamics) {
    message("Expected dynamics rows: ", format(summary$dynamics_rows, big.mark = ","))
  }
  if (summary$save_resources) {
    message("Expected resource rows: ", format(summary$resource_rows, big.mark = ","))
  }
  message("Expected temperature rows: ", format(summary$temperature_rows, big.mark = ","))
  if (summary$save_dynamics) {
    message("Estimated dynamics.db size: ", format_bytes(summary$estimated_dynamics_db_bytes))
  }
  if (summary$save_resources) {
    message("Estimated resources.db size: ", format_bytes(summary$estimated_resources_db_bytes))
  }
  message("Estimated temperatures.db size: ", format_bytes(summary$estimated_temperatures_db_bytes))
  message("Estimated total DB size: ", format_bytes(summary$estimated_total_db_bytes))
  message("Estimated runtime: ", format_duration(summary$estimated_total_seconds))
  if (summary$parallel_environments || summary$parallel_simulations || summary$parallel_community_measures) {
    message("Parallel workers: ", summary$parallel_workers)
    message("Parallel environment generation: ", summary$parallel_environments)
    message("Parallel simulations: ", summary$parallel_simulations)
    message("Parallel community measures: ", summary$parallel_community_measures)
  } else {
    message("Parallel workers: not enabled")
  }
  message("")

  answer <- readline("Continue with simulation and analysis? [y/N] ")
  tolower(trimws(answer)) %in% c("y", "yes")
}

append_experiment_log_entry <- function(log_path, event, fields = list()) {
  entry <- c(
    list(
      timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      event = event
    ),
    fields
  )
  cat(
    jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", na = "null"),
    "\n",
    file = log_path,
    append = TRUE
  )

  invisible(log_path)
}

read_experiment_specification_for_log <- function(design_path) {
  parsed <- try(
    yaml::read_yaml(design_path),
    silent = TRUE
  )
  if (!inherits(parsed, "try-error")) {
    return(list(specification = parsed))
  }

  list(
    specification_parse_error = conditionMessage(attr(parsed, "condition")),
    specification_text = paste(readLines(design_path, warn = FALSE), collapse = "\n")
  )
}

initialise_experiment_log <- function(log_path,
                                      experiment_folder,
                                      experiment_name,
                                      experiment_design_filename,
                                      design_path,
                                      overwrite,
                                      verbose) {
  prepare_output_path(
    log_path,
    overwrite = overwrite,
    verbose = verbose,
    label = "experiment log"
  )

  file.create(log_path)
  append_experiment_log_entry(
    log_path,
    "workflow_started",
    list(
      log_format = "newline_delimited_json",
      log_format_version = 1,
      message = "Experiment workflow started.",
      experiment_name = experiment_name,
      experiment_folder = experiment_folder,
      experiment_design_filename = experiment_design_filename,
      design_path = design_path,
      r_version = paste(R.version$major, R.version$minor, sep = "."),
      platform = R.version$platform
    )
  )
  append_experiment_log_entry(
    log_path,
    "experiment_specification",
    c(
      list(
        message = "Experiment specification read from the design YAML file.",
        experiment_design_filename = experiment_design_filename
      ),
      read_experiment_specification_for_log(design_path)
    )
  )

  invisible(log_path)
}

run_logged_experiment_step <- function(step_name, log_path, code) {
  started_at <- Sys.time()
  append_experiment_log_entry(
    log_path,
    "step_started",
    list(
      step = step_name,
      message = paste0("Started ", step_name, ".")
    )
  )

  error <- NULL
  result <- tryCatch(
    force(code),
    error = function(e) {
      error <<- e
      NULL
    }
  )

  finished_at <- Sys.time()
  elapsed_seconds <- as.numeric(difftime(finished_at, started_at, units = "secs"))
  if (is.null(error)) {
    append_experiment_log_entry(
      log_path,
      "step_completed",
      list(
        step = step_name,
        message = paste0("Completed ", step_name, "."),
        elapsed_seconds = elapsed_seconds,
        elapsed = format_duration(elapsed_seconds)
      )
    )
    return(result)
  }

  append_experiment_log_entry(
    log_path,
    "step_failed",
    list(
      step = step_name,
      message = paste0("Failed ", step_name, "."),
      elapsed_seconds = elapsed_seconds,
      elapsed = format_duration(elapsed_seconds),
      error = conditionMessage(error)
    )
  )
  stop(conditionMessage(error), call. = FALSE)
}

estimate_experiment_outputs_from_spec <- function(experiment_folder, spec) {
  expt <- readRDS(file.path(experiment_folder, "experiment_table.RDS"))
  settings <- flatten_spec_settings(spec)

  integration_steps <- settings$burn_in_duration + settings$experiment_duration + 1L
  output_times <- seq_len(integration_steps)
  output_times <- output_times[
    ((output_times - 1L) %% settings$dynamics_save_every) == 0L |
      output_times == max(output_times)
  ]
  saved_time_points <- sum(output_times > settings$burn_in_duration)

  species_per_case <- vapply(expt$community_object, function(x) x$S, numeric(1))
  resource_per_case <- vapply(expt$community_object, function(x) {
    if (is.null(x$R)) {
      return(0)
    }
    x$R
  }, numeric(1))

  resource_output_times <- seq_len(integration_steps)
  resource_output_times <- resource_output_times[
    ((resource_output_times - 1L) %% settings$resources_save_every) == 0L |
      resource_output_times == max(resource_output_times)
  ]
  saved_resource_time_points <- sum(resource_output_times > settings$burn_in_duration)

  save_resources <- settings$save_resources &&
    settings$dynamics_type == "consumer_resource_continuous"
  dynamics_rows <- if (settings$save_dynamics) {
    saved_time_points * sum(species_per_case)
  } else {
    0
  }
  resource_rows <- if (save_resources) {
    saved_resource_time_points * sum(resource_per_case)
  } else {
    0
  }

  env_series_count <- dplyr::n_distinct(expt$env_series_id)
  temperature_rows <- env_series_count * (settings$experiment_duration + 1L)

  estimated_dynamics_db_bytes <- dynamics_rows * 45
  estimated_resources_db_bytes <- resource_rows * 45
  estimated_temperatures_db_bytes <- temperature_rows * 55
  estimated_total_db_bytes <- estimated_dynamics_db_bytes +
    estimated_resources_db_bytes +
    estimated_temperatures_db_bytes

  rates <- runtime_estimator_rates(settings$dynamics_type)
  effective_workers <- if (settings$parallel_simulations) settings$parallel_workers else 1L
  estimated_experiment_table_seconds <- max(
    rates$experiment_table_fixed_seconds,
    nrow(expt) * rates$experiment_table_seconds_per_case
  )
  effective_environment_workers <- if (settings$parallel_environments) {
    max(1L, min(settings$parallel_workers, env_series_count))
  } else {
    1L
  }
  estimated_environment_seconds <- 0.5 +
    temperature_rows * 0.00029 / effective_environment_workers
  reference_parallel_workers <- 29
  estimated_simulation_seconds <- (
    nrow(expt) *
      integration_steps *
      rates$simulation_seconds_per_case_step *
      reference_parallel_workers
  ) / effective_workers
  effective_community_measure_workers <- if (settings$parallel_community_measures) {
    max(1, min(settings$parallel_workers, nrow(expt)) * 0.5)
  } else {
    1
  }
  estimated_community_measure_seconds <- (
    nrow(expt) * rates$community_measure_seconds_per_case
  ) / effective_community_measure_workers
  estimated_io_seconds <- (dynamics_rows + resource_rows) * 0.00001
  estimated_total_seconds <- estimated_experiment_table_seconds +
    estimated_environment_seconds +
    estimated_simulation_seconds +
    estimated_community_measure_seconds +
    estimated_io_seconds

  list(
    n_cases = nrow(expt),
    dynamics_type = settings$dynamics_type,
    integration_steps = integration_steps,
    saved_time_points = saved_time_points,
    saved_resource_time_points = saved_resource_time_points,
    dynamics_save_every = settings$dynamics_save_every,
    resources_save_every = settings$resources_save_every,
    summary_checkpoint_every = settings$summary_checkpoint_every,
    runtime_update_every = settings$runtime_update_every,
    save_dynamics = settings$save_dynamics,
    save_resources = save_resources,
    dynamics_rows = dynamics_rows,
    resource_rows = resource_rows,
    temperature_rows = temperature_rows,
    parallel_environments = settings$parallel_environments,
    parallel_simulations = settings$parallel_simulations,
    parallel_community_measures = settings$parallel_community_measures,
    parallel_workers = settings$parallel_workers,
    estimated_dynamics_db_bytes = estimated_dynamics_db_bytes,
    estimated_resources_db_bytes = estimated_resources_db_bytes,
    estimated_temperatures_db_bytes = estimated_temperatures_db_bytes,
    estimated_total_db_bytes = estimated_total_db_bytes,
    estimated_total_seconds = estimated_total_seconds
  )
}

#' Run a complete experiment workflow
#'
#' This is the standard YAML experiment workflow. It creates or locates the
#' experiment folder, reads the YAML experiment specification from that folder,
#' builds the canonical experiment table, generates environmental time series,
#' simulates dynamics, and calculates community-level summary measures.
#'
#' @param experiment_folder_location Location where the experiment folder should
#'   be created.
#' @param experiment_name Name of the experiment folder.
#' @param experiment_design_filename Name of the experiment definition file.
#'   This must be a `.yaml` or `.yml` file already present inside the experiment
#'   folder.
#' @param overwrite Logical. If `TRUE`, overwrite existing workflow outputs.
#' @param verbose Logical. If `TRUE`, print progress messages during the
#'   workflow.
#' @param confirm_run Logical. If `TRUE`, show a preflight summary and ask
#'   whether to continue before creating environments and simulating dynamics. Defaults to
#'   `interactive()`.
#'
#' @details The preflight summary estimates output rows, database sizes, and
#'   total runtime from the canonical experiment table and YAML settings.
#'   Confirmation happens before environment generation, dynamics simulation,
#'   and analysis. Each run also writes `experiment_log.txt`, a plain-text
#'   newline-delimited JSON log containing the parsed YAML specification,
#'   preflight summary, output paths, workflow status, and elapsed time for each
#'   workflow step.
#'
#' @return Invisibly returns a named list containing the experiment folder and
#'   the main output file paths.
#' @export
#'
#' @examples NULL
run_experiment <- function(experiment_folder_location,
                           experiment_name,
                           experiment_design_filename,
                           overwrite = FALSE,
                           verbose = TRUE,
                           confirm_run = interactive()) {

  if (verbose) {
    message("Creating or locating experiment folder")
  }
  experiment_folder <- create_experiment_folder(
    experiment_folder_location = experiment_folder_location,
    experiment_name = experiment_name,
    verbose = verbose
  )
  design_path <- file.path(experiment_folder, experiment_design_filename)

  if (!file.exists(design_path)) {
    stop(
      paste0(
        "Experiment design file not found: ", design_path,
        "\nCopy the YAML file into the experiment folder before calling run_experiment()."
      ),
      call. = FALSE
    )
  }
  if (!tolower(tools::file_ext(design_path)) %in% c("yaml", "yml")) {
    stop(
      "`experiment_design_filename` must point to a `.yaml` or `.yml` file.",
      call. = FALSE
    )
  }
  spec <- read_experiment_spec(design_path)

  experiment_log <- file.path(experiment_folder, "experiment_log.txt")
  initialise_experiment_log(
    log_path = experiment_log,
    experiment_folder = experiment_folder,
    experiment_name = experiment_name,
    experiment_design_filename = experiment_design_filename,
    design_path = design_path,
    overwrite = overwrite,
    verbose = verbose
  )

  workflow_started_at <- Sys.time()
  workflow_error <- NULL
  outputs <- tryCatch({
    if (verbose) {
      message("Creating experiment table")
    }
    run_logged_experiment_step("create_experiment_table", experiment_log, {
      create_experiment_table_from_spec(
        spec,
        output_path = file.path(experiment_folder, "experiment_table.RDS"),
        overwrite = overwrite,
        verbose = verbose
      )
    })

    experiment_summary <- run_logged_experiment_step(
      "estimate_experiment_outputs",
      experiment_log,
      {
        estimate_experiment_outputs_from_spec(
          experiment_folder,
          spec
        )
      }
    )
    append_experiment_log_entry(
      experiment_log,
      "experiment_preflight_summary",
      list(
        message = "Experiment output and runtime estimates.",
        summary = experiment_summary
      )
    )

    if (confirm_run) {
      if (!interactive()) {
        warning(
          "`confirm_run = TRUE`, but the session is not interactive. ",
          "Skipping confirmation prompt.",
          call. = FALSE
        )
      } else if (!confirm_experiment_run(experiment_summary)) {
        stop("Experiment run cancelled by user.", call. = FALSE)
      }
    }

    if (verbose) {
      message("Creating environments")
    }
    run_logged_experiment_step("create_environments", experiment_log, {
      create_environments_from_spec(
        experiment_folder,
        spec,
        overwrite = overwrite,
        verbose = verbose
      )
    })

    if (verbose) {
      message("Simulating dynamics")
    }
    run_logged_experiment_step("simulate_dynamics", experiment_log, {
      simulate_dynamics_from_spec(
        experiment_folder,
        spec,
        overwrite = overwrite,
        verbose = verbose
      )
    })

    if (verbose) {
      message("Calculating community measures")
    }
    run_logged_experiment_step("get_community_measures", experiment_log, {
      get_community_measures_from_spec(
        experiment_folder,
        spec,
        overwrite = overwrite,
        verbose = verbose
      )
    })

    outputs <- list(
      experiment_folder = experiment_folder,
      experiment_log = experiment_log,
      experiment_table = file.path(experiment_folder, "experiment_table.RDS"),
      temperatures_db = file.path(experiment_folder, "temperatures.db"),
      simulation_summaries = file.path(experiment_folder, "simulation_summaries.RDS"),
      population_summaries = file.path(experiment_folder, "population_summaries.RDS"),
      community_measures = file.path(experiment_folder, "community_measures.RDS")
    )
    if (experiment_summary$save_dynamics) {
      outputs$dynamics_db <- file.path(experiment_folder, "dynamics.db")
    }
    if (experiment_summary$save_resources) {
      outputs$resources_db <- file.path(experiment_folder, "resources.db")
    }

    outputs
  }, error = function(e) {
    workflow_error <<- e
    NULL
  })

  workflow_finished_at <- Sys.time()
  workflow_elapsed_seconds <- as.numeric(
    difftime(workflow_finished_at, workflow_started_at, units = "secs")
  )
  append_experiment_log_entry(
    experiment_log,
    if (is.null(workflow_error)) "workflow_completed" else "workflow_failed",
    list(
      message = if (is.null(workflow_error)) {
        "Experiment workflow complete."
      } else {
        "Experiment workflow failed."
      },
      elapsed_seconds = workflow_elapsed_seconds,
      elapsed = format_duration(workflow_elapsed_seconds),
      outputs = outputs,
      error = if (is.null(workflow_error)) NULL else conditionMessage(workflow_error)
    )
  )

  if (!is.null(workflow_error)) {
    stop(conditionMessage(workflow_error), call. = FALSE)
  }

  if (verbose) {
    message("Experiment workflow complete")
    message("Wrote experiment log: ", experiment_log)
  }

  invisible(outputs)
}
