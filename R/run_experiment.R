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

get_json_design_value <- function(expt_def, name, default) {
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

estimate_experiment_outputs <- function(experiment_folder, experiment_design_filename) {
  expt <- readRDS(file.path(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(file.path(experiment_folder, experiment_design_filename))

  burn_in_duration <- as.integer(get_json_design_value(expt_def, "burn_in_duration", 0))
  experiment_duration <- as.integer(get_json_design_value(expt_def, "experiment_duration", 0))
  dynamics_save_every <- as.integer(get_json_design_value(expt_def, "dynamics_save_every", 1))
  if (is.na(dynamics_save_every) || dynamics_save_every < 1) {
    dynamics_save_every <- 1L
  }
  dynamics_type <- get_json_design_value(expt_def, "dynamics_type", "discrete")
  parallel_simulations <- isTRUE(get_json_design_value(expt_def, "parallel_simulations", FALSE))
  parallel_workers <- as.integer(get_json_design_value(
    expt_def,
    "parallel_workers",
    max(1, parallel::detectCores(logical = FALSE) - 1)
  ))
  if (is.na(parallel_workers)) {
    parallel_workers <- 1L
  }
  parallel_workers <- max(1L, min(parallel_workers, nrow(expt)))

  integration_steps <- burn_in_duration + experiment_duration + 1L
  output_times <- seq_len(integration_steps)
  output_times <- output_times[
    ((output_times - 1L) %% dynamics_save_every) == 0L |
      output_times == max(output_times)
  ]
  saved_time_points <- sum(output_times > burn_in_duration)

  species_per_case <- vapply(expt$community_object, function(x) x$S, numeric(1))
  resource_per_case <- vapply(expt$community_object, function(x) {
    if (is.null(x$R)) {
      return(0)
    }
    x$R
  }, numeric(1))

  dynamics_rows <- saved_time_points * sum(species_per_case)
  resource_rows <- if (dynamics_type == "consumer_resource_continuous") {
    saved_time_points * sum(resource_per_case)
  } else {
    0
  }

  env_series_count <- dplyr::n_distinct(expt$env_series_id)
  temperature_rows <- env_series_count * (experiment_duration + 1L)

  estimated_dynamics_db_bytes <- dynamics_rows * 45
  estimated_resources_db_bytes <- resource_rows * 45
  estimated_temperatures_db_bytes <- temperature_rows * 55
  estimated_total_db_bytes <- estimated_dynamics_db_bytes +
    estimated_resources_db_bytes +
    estimated_temperatures_db_bytes

  seconds_per_case_step <- switch(
    dynamics_type,
    discrete = 0.000015,
    continuous = 0.00005,
    consumer_resource_continuous = 0.00008,
    0.00005
  )
  effective_workers <- if (parallel_simulations) parallel_workers else 1L
  estimated_simulation_seconds <- (
    nrow(expt) * integration_steps * seconds_per_case_step
  ) / effective_workers
  estimated_io_seconds <- (dynamics_rows + resource_rows) * 0.00001
  estimated_total_seconds <- estimated_simulation_seconds + estimated_io_seconds

  list(
    n_cases = nrow(expt),
    dynamics_type = dynamics_type,
    integration_steps = integration_steps,
    saved_time_points = saved_time_points,
    dynamics_save_every = dynamics_save_every,
    dynamics_rows = dynamics_rows,
    resource_rows = resource_rows,
    temperature_rows = temperature_rows,
    parallel_simulations = parallel_simulations,
    parallel_workers = parallel_workers,
    estimated_dynamics_db_bytes = estimated_dynamics_db_bytes,
    estimated_resources_db_bytes = estimated_resources_db_bytes,
    estimated_temperatures_db_bytes = estimated_temperatures_db_bytes,
    estimated_total_db_bytes = estimated_total_db_bytes,
    estimated_total_seconds = estimated_total_seconds
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
  message("dynamics_save_every: ", summary$dynamics_save_every)
  message("Expected dynamics rows: ", format(summary$dynamics_rows, big.mark = ","))
  if (summary$resource_rows > 0) {
    message("Expected resource rows: ", format(summary$resource_rows, big.mark = ","))
  }
  message("Expected temperature rows: ", format(summary$temperature_rows, big.mark = ","))
  message("Estimated dynamics.db size: ", format_bytes(summary$estimated_dynamics_db_bytes))
  if (summary$resource_rows > 0) {
    message("Estimated resources.db size: ", format_bytes(summary$estimated_resources_db_bytes))
  }
  message("Estimated temperatures.db size: ", format_bytes(summary$estimated_temperatures_db_bytes))
  message("Estimated total DB size: ", format_bytes(summary$estimated_total_db_bytes))
  message("Estimated runtime: ", format_duration(summary$estimated_total_seconds))
  if (summary$parallel_simulations) {
    message("Parallel workers: ", summary$parallel_workers)
  } else {
    message("Parallel workers: not enabled")
  }
  message("")

  answer <- readline("Continue with simulation and analysis? [y/N] ")
  tolower(trimws(answer)) %in% c("y", "yes")
}

#' Run a complete experiment workflow
#'
#' This is a convenience wrapper for the standard experiment workflow. It
#' creates the experiment folder, builds the experiment table, generates
#' environmental time series, simulates dynamics, and calculates community-level
#' summary measures.
#'
#' @param experiment_folder_location Location where the experiment folder should
#'   be created.
#' @param experiment_name Name of the experiment folder.
#' @param experiment_design_filename Name of the experiment definition file.
#'   This file must already be present inside the experiment folder.
#' @param overwrite Logical. If `TRUE`, overwrite existing workflow outputs.
#' @param verbose Logical. If `TRUE`, print progress messages during the
#'   workflow.
#' @param confirm_run Logical. If `TRUE`, show a preflight summary and ask
#'   whether to continue before creating environments and simulating dynamics. Defaults to
#'   `interactive()`.
#'
#' @details The preflight summary estimates output rows, database sizes, and
#'   runtime from the experiment table and design settings. Confirmation happens
#'   before environment generation, dynamics simulation, and analysis. These
#'   estimates are intended as rough guidance before launching large experiments.
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
        "\nCopy the JSON file into the experiment folder before calling run_experiment()."
      ),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Creating experiment table")
  }
  create_experiment_table(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  experiment_summary <- estimate_experiment_outputs(
    experiment_folder,
    experiment_design_filename
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
  create_environments(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message("Simulating dynamics")
  }
  simulate_dynamics(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message("Calculating community measures")
  }
  get_community_measures(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  outputs <- list(
    experiment_folder = experiment_folder,
    experiment_table = file.path(experiment_folder, "experiment_table.RDS"),
    temperatures_db = file.path(experiment_folder, "temperatures.db"),
    dynamics_db = file.path(experiment_folder, "dynamics.db"),
    community_measures = file.path(experiment_folder, "community_measures.RDS")
  )

  if (verbose) {
    message("Experiment workflow complete")
  }

  invisible(outputs)
}
