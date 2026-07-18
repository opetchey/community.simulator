spec_setting <- function(spec, section, name, default = NULL) {
  value <- spec[[section]][[name]]
  if (is.null(value)) {
    return(default)
  }
  value
}

spec_ode_setting <- function(spec, name, default) {
  value <- spec$simulation$ode[[name]]
  if (is.null(value)) {
    return(default)
  }
  value
}

numeric_spec_setting <- function(value, name) {
  value <- as.numeric(value)
  if (length(value) != 1 || is.na(value) || !is.finite(value)) {
    stop("`", name, "` must be one finite number.", call. = FALSE)
  }
  value
}

integer_spec_setting <- function(value, name) {
  value <- as.integer(value)
  if (length(value) != 1 || is.na(value) || !is.finite(value)) {
    stop("`", name, "` must be one integer.", call. = FALSE)
  }
  value
}

worker_spec_setting <- function(value, name) {
  if (is.character(value) && length(value) == 1) {
    value <- switch(
      value,
      available_cores_minus_1 = max(1L, parallel::detectCores(logical = FALSE) - 1L),
      available_cores = max(1L, parallel::detectCores(logical = FALSE)),
      auto = max(1L, parallel::detectCores(logical = FALSE) - 1L),
      stop(
        "`",
        name,
        "` must be a positive integer, `available_cores`, ",
        "`available_cores_minus_1`, or `auto`.",
        call. = FALSE
      )
    )
    return(as.integer(value))
  }
  integer_spec_setting(value, name)
}

flatten_spec_settings <- function(spec) {
  list(
    model_type = spec$model$type,
    dynamics_type = normalize_model_type(spec$model$type),
    burn_in_duration = integer_spec_setting(spec$simulation$burn_in_duration, "simulation.burn_in_duration"),
    experiment_duration = integer_spec_setting(spec$simulation$experiment_duration, "simulation.experiment_duration"),
    temperature_mean = numeric_spec_setting(spec$environment$temperature$mean, "environment.temperature.mean"),
    temperature_sd = numeric_spec_setting(spec$environment$temperature$sd, "environment.temperature.sd"),
    one_over_f_gamma = numeric_spec_setting(spec$environment$temperature$one_over_f_gamma, "environment.temperature.one_over_f_gamma"),
    temperature_interpolation = spec_setting(spec, "simulation", "temperature_interpolation", "linear"),
    immigration_rate = numeric_spec_setting(spec_setting(spec, "simulation", "immigration_rate", 0.1), "simulation.immigration_rate"),
    consumer_immigration_rate = numeric_spec_setting(spec_setting(spec, "simulation", "consumer_immigration_rate", spec_setting(spec, "simulation", "immigration_rate", 0.1)), "simulation.consumer_immigration_rate"),
    initial_consumer_total_abundance = numeric_spec_setting(spec_setting(spec, "simulation", "initial_consumer_total_abundance", 1000), "simulation.initial_consumer_total_abundance"),
    resource_initial_value = numeric_spec_setting(spec_setting(spec, "simulation", "resource_initial_value", 100), "simulation.resource_initial_value"),
    immigration_mode = spec_setting(spec, "simulation", "immigration_mode", "continuous"),
    ode_method = spec_ode_setting(spec, "method", "lsoda"),
    ode_rtol = numeric_spec_setting(spec_ode_setting(spec, "rtol", 1e-6), "simulation.ode.rtol"),
    ode_atol = numeric_spec_setting(spec_ode_setting(spec, "atol", 1e-8), "simulation.ode.atol"),
    ode_max_step = numeric_spec_setting(spec_ode_setting(spec, "max_step", 1), "simulation.ode.max_step"),
    blowup_threshold = numeric_spec_setting(spec_setting(spec, "simulation", "blowup_threshold", 1e12), "simulation.blowup_threshold"),
    negative_tolerance = numeric_spec_setting(spec_setting(spec, "simulation", "negative_tolerance", 1e-8), "simulation.negative_tolerance"),
    dynamics_save_every = integer_spec_setting(spec_setting(spec, "output", "dynamics_save_every", 1), "output.dynamics_save_every"),
    resources_save_every = integer_spec_setting(spec_setting(spec, "output", "resources_save_every", spec_setting(spec, "output", "dynamics_save_every", 1)), "output.resources_save_every"),
    save_dynamics = isTRUE(spec_setting(spec, "output", "save_dynamics", TRUE)),
    save_resources = isTRUE(spec_setting(spec, "output", "save_resources", TRUE)),
    extinction_threshold = numeric_spec_setting(spec_setting(spec, "measures", "extinction_threshold", 1e-8), "measures.extinction_threshold"),
    summary_checkpoint_every = integer_spec_setting(spec_setting(spec, "output", "summary_checkpoint_every", 1), "output.summary_checkpoint_every"),
    runtime_update_every = integer_spec_setting(spec_setting(spec, "output", "runtime_update_every", 1), "output.runtime_update_every"),
    simulation_progress = isTRUE(spec_setting(spec, "output", "simulation_progress", FALSE)),
    environment_progress = isTRUE(spec_setting(spec, "output", "environment_progress", FALSE)),
    parallel_workers = worker_spec_setting(spec_setting(spec, "parallel", "workers", 1), "parallel.workers"),
    parallel_environments = isTRUE(spec_setting(spec, "parallel", "environments", FALSE)),
    parallel_simulations = isTRUE(spec_setting(spec, "parallel", "simulations", FALSE)),
    parallel_community_measures = isTRUE(spec_setting(spec, "parallel", "community_measures", FALSE)),
    initial_abundance_seed_base = spec_setting(spec, "simulation", "initial_abundance_seed_base", NULL),
    soft_viability_scale = numeric_spec_setting(spec_setting(spec, "measures", "soft_viability_scale", 0.01), "measures.soft_viability_scale")
  )
}

#' Create environments from a YAML experiment specification
#'
#' @param experiment_folder Folder containing `experiment_table.RDS` and where
#'   `temperatures.db` should be written.
#' @param spec A YAML experiment specification path or specification object.
#' @param overwrite Logical. If `TRUE`, overwrite existing outputs.
#' @param verbose Logical. If `TRUE`, print progress messages.
#'
#' @return Invisibly returns the path to `temperatures.db`.
#' @export
create_environments_from_spec <- function(experiment_folder,
                                          spec,
                                          overwrite = FALSE,
                                          verbose = TRUE) {
  require_dbplyr()
  spec <- coerce_experiment_spec(spec)
  settings <- flatten_spec_settings(spec)
  experiment_folder <- path.expand(experiment_folder)
  output_path <- file.path(experiment_folder, "temperatures.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "temperatures database")

  expt <- readRDS(file.path(experiment_folder, "experiment_table.RDS"))
  environments <- expt |>
    dplyr::select(
      env_series_id,
      temperature_mean,
      temperature_sd,
      one_over_f_gamma,
      temperature_seed
    ) |>
    dplyr::distinct()

  create_environment <- function(i) {
    set.seed(environments$temperature_seed[i])
    temperature_mean <- environments$temperature_mean[i]
    temperature_sd <- environments$temperature_sd[i]
    one_over_f_gamma <- environments$one_over_f_gamma[i]

    tibble::tibble(
      phase = c(
        rep("burn_in", settings$burn_in_duration),
        rep("expt", settings$experiment_duration + 1)
      ),
      time = 0:(settings$burn_in_duration + settings$experiment_duration),
      temperature = c(
        rep(temperature_mean, settings$burn_in_duration),
        as.numeric(
          scale(primer::one_over_f(
            gamma = one_over_f_gamma,
            N = settings$experiment_duration + 1
          ))
        ) * temperature_sd + temperature_mean
      ),
      env_series_id = environments$env_series_id[i]
    ) |>
      dplyr::mutate(temperature = ifelse(
        phase == "burn_in",
        temperature[settings$burn_in_duration + 1],
        temperature
      )) |>
      dplyr::filter(.data$time > settings$burn_in_duration)
  }

  conn <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  table_written <- FALSE

  for (i in seq_len(nrow(environments))) {
    if (isTRUE(verbose)) {
      message("Creating environment ", i, " of ", nrow(environments))
    }
    DBI::dbWriteTable(
      conn,
      "temperatures",
      create_environment(i),
      overwrite = !table_written,
      append = table_written
    )
    table_written <- TRUE
  }

  announce_output_written(output_path, verbose = verbose, label = "temperatures database")
  invisible(output_path)
}

#' Simulate dynamics from a YAML experiment specification
#'
#' @inheritParams create_environments_from_spec
#'
#' @return Invisibly returns the paths to simulation outputs.
#' @export
simulate_dynamics_from_spec <- function(experiment_folder,
                                        spec,
                                        overwrite = FALSE,
                                        verbose = TRUE) {
  require_dbplyr()
  spec <- coerce_experiment_spec(spec)
  settings <- flatten_spec_settings(spec)
  experiment_folder <- path.expand(experiment_folder)

  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), file.path(experiment_folder, "temperatures.db"))
  on.exit(DBI::dbDisconnect(conn_temperatures), add = TRUE)
  temperatures_data <- dplyr::tbl(conn_temperatures, "temperatures") |>
    dplyr::collect()
  expt <- readRDS(file.path(experiment_folder, "experiment_table.RDS"))

  if (!settings$dynamics_type %in% c("discrete", "continuous", "consumer_resource_continuous")) {
    stop(
      "`model.type` must be 'lv_discrete', 'lv_continuous', or ",
      "'consumer_resource_continuous'.",
      call. = FALSE
    )
  }
  if (is.na(settings$dynamics_save_every) || settings$dynamics_save_every < 1) {
    stop("`output.dynamics_save_every` must be an integer >= 1.", call. = FALSE)
  }
  if (is.na(settings$resources_save_every) || settings$resources_save_every < 1) {
    stop("`output.resources_save_every` must be an integer >= 1.", call. = FALSE)
  }

  summaries_path <- file.path(experiment_folder, "simulation_summaries.RDS")
  population_summaries_path <- file.path(experiment_folder, "population_summaries.RDS")
  prepare_output_path(summaries_path, overwrite = overwrite, verbose = verbose, label = "simulation summaries file")
  prepare_output_path(population_summaries_path, overwrite = overwrite, verbose = verbose, label = "population summaries file")

  save_resources <- settings$save_resources &&
    settings$dynamics_type == "consumer_resource_continuous"

  conn_dynamics <- NULL
  dynamics_path <- file.path(experiment_folder, "dynamics.db")
  if (settings$save_dynamics) {
    prepare_output_path(dynamics_path, overwrite = overwrite, verbose = verbose, label = "dynamics database")
    conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), dynamics_path)
    on.exit({
      if (!is.null(conn_dynamics) && DBI::dbIsValid(conn_dynamics)) {
        DBI::dbDisconnect(conn_dynamics)
      }
    }, add = TRUE)
  }

  conn_resources <- NULL
  resources_path <- file.path(experiment_folder, "resources.db")
  if (save_resources) {
    prepare_output_path(resources_path, overwrite = overwrite, verbose = verbose, label = "resources database")
    conn_resources <- DBI::dbConnect(RSQLite::SQLite(), resources_path)
    on.exit({
      if (!is.null(conn_resources) && DBI::dbIsValid(conn_resources)) {
        DBI::dbDisconnect(conn_resources)
      }
    }, add = TRUE)
  }

  expt_def <- list(
    burn_in_duration = settings$burn_in_duration,
    experiment_duration = settings$experiment_duration,
    temperature_mean = settings$temperature_mean
  )

  dynamics_written <- FALSE
  resources_written <- FALSE
  case_summaries <- vector("list", nrow(expt))
  population_summaries <- vector("list", nrow(expt))

  for (i in seq_len(nrow(expt))) {
    if (isTRUE(verbose)) {
      message("Simulating case ", i, " of ", nrow(expt))
    }
    case_result <- simulate_one_dynamics_case(
      i = i,
      expt = expt,
      temperatures_data = temperatures_data,
      expt_def = expt_def,
      dynamics_type = settings$dynamics_type,
      temperature_interpolation = settings$temperature_interpolation,
      immigration_rate = settings$immigration_rate,
      consumer_immigration_rate = settings$consumer_immigration_rate,
      initial_consumer_total_abundance = settings$initial_consumer_total_abundance,
      resource_initial_value = settings$resource_initial_value,
      immigration_mode = settings$immigration_mode,
      ode_method = settings$ode_method,
      ode_rtol = settings$ode_rtol,
      ode_atol = settings$ode_atol,
      ode_max_step = settings$ode_max_step,
      blowup_threshold = settings$blowup_threshold,
      negative_tolerance = settings$negative_tolerance,
      dynamics_save_every = settings$dynamics_save_every,
      resources_save_every = settings$resources_save_every,
      save_dynamics = settings$save_dynamics,
      save_resources = save_resources,
      extinction_threshold = settings$extinction_threshold,
      initial_abundance_seed_base = settings$initial_abundance_seed_base
    )
    case_summaries[[i]] <- case_result$case_summary
    population_summaries[[i]] <- case_result$population_summary

    if (!is.null(conn_dynamics) && !is.null(case_result$dynamics)) {
      DBI::dbWriteTable(
        conn_dynamics,
        "dynamics",
        case_result$dynamics,
        overwrite = !dynamics_written,
        append = dynamics_written
      )
      dynamics_written <- TRUE
    }
    if (!is.null(conn_resources) && !is.null(case_result$resources)) {
      DBI::dbWriteTable(
        conn_resources,
        "resources",
        case_result$resources,
        overwrite = !resources_written,
        append = resources_written
      )
      resources_written <- TRUE
    }
  }

  saveRDS(dplyr::bind_rows(case_summaries), summaries_path)
  saveRDS(dplyr::bind_rows(population_summaries), population_summaries_path)
  announce_output_written(summaries_path, verbose = verbose, label = "simulation summaries file")
  announce_output_written(population_summaries_path, verbose = verbose, label = "population summaries file")

  invisible(list(
    simulation_summaries = summaries_path,
    population_summaries = population_summaries_path,
    dynamics_db = if (settings$save_dynamics) dynamics_path else NULL,
    resources_db = if (save_resources) resources_path else NULL
  ))
}

#' Calculate community measures from a YAML experiment specification
#'
#' @inheritParams create_environments_from_spec
#'
#' @return Invisibly returns the path to `community_measures.RDS`.
#' @export
get_community_measures_from_spec <- function(experiment_folder,
                                             spec,
                                             overwrite = FALSE,
                                             verbose = TRUE) {
  require_dbplyr()
  spec <- coerce_experiment_spec(spec)
  settings <- flatten_spec_settings(spec)
  experiment_folder <- path.expand(experiment_folder)
  output_path <- file.path(experiment_folder, "community_measures.RDS")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "community-measures file")

  expt <- readRDS(file.path(experiment_folder, "experiment_table.RDS"))
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), file.path(experiment_folder, "temperatures.db"))
  on.exit(DBI::dbDisconnect(conn_temperatures), add = TRUE)
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")

  summaries_path <- file.path(experiment_folder, "simulation_summaries.RDS")
  if (!file.exists(summaries_path)) {
    stop("Cannot calculate community measures because simulation_summaries.RDS was not found.", call. = FALSE)
  }
  comm_dynamic_measures <- readRDS(summaries_path) |>
    tibble::as_tibble()
  performance_optimum_measures <- get_community_performance_optimum_measures(
    temperatures,
    expt
  )
  comm_cpc <- get_community_CPC_measures(
    temperatures,
    expt,
    flatten_spec_settings(spec),
    every_t = 1,
    soft_viability_scale = settings$soft_viability_scale,
    parallel_community_measures = settings$parallel_community_measures,
    parallel_workers = settings$parallel_workers,
    verbose = verbose
  )

  comm_measures <- expt |>
    dplyr::left_join(comm_dynamic_measures, by = "case_id") |>
    dplyr::left_join(performance_optimum_measures, by = "case_id") |>
    dplyr::left_join(comm_cpc, by = "case_id") |>
    standardize_community_measure_names()

  saveRDS(comm_measures, output_path)
  announce_output_written(output_path, verbose = verbose, label = "community-measures file")
  invisible(output_path)
}

standardize_community_measure_names <- function(x) {
  rename_if_present <- function(data, old, new) {
    if (old %in% names(data) && !new %in% names(data)) {
      names(data)[names(data) == old] <- new
    }
    data
  }

  renames <- list(
    mean_totab = "mean_total_abundance",
    sd_totab = "sd_total_abundance",
    CV_totab = "cv_total_abundance",
    sync_ab = "synchrony_abundance",
    pop_CV_ab = "mean_population_cv_abundance",
    final_totab = "final_total_abundance",
    sum_rel_b_opt = "sum_relative_performance_optimum",
    min_rel_b_opt = "minimum_absolute_relative_performance_optimum",
    real_mean_b_opt = "realized_mean_performance_optimum",
    real_sd_b_opt = "realized_sd_performance_optimum",
    CV_community_perf_info = "cv_community_performance_info",
    CV_community_perf_naive = "cv_community_performance_naive",
    CV_community_viability_binary_info = "cv_community_viability_binary_info",
    CV_community_viability_binary_naive = "cv_community_viability_binary_naive",
    CV_community_viability_soft_info = "cv_community_viability_soft_info",
    CV_community_viability_soft_naive = "cv_community_viability_soft_naive",
    synchrony_perf_info = "synchrony_performance_info",
    synchrony_perf_naive = "synchrony_performance_naive",
    avg_perf_CV_info = "mean_species_cv_performance_info",
    avg_perf_CV_naive = "mean_species_cv_performance_naive"
  )

  for (old in names(renames)) {
    x <- rename_if_present(x, old, renames[[old]])
  }
  x
}

# Internal convenience helper used during the rewrite. The public experiment
# runner is `run_experiment()`.
run_experiment_from_spec <- function(spec_path,
                                     experiment_folder,
                                     overwrite = FALSE,
                                     verbose = TRUE) {
  spec <- read_experiment_spec(spec_path)
  experiment_folder <- path.expand(experiment_folder)
  if (!dir.exists(experiment_folder)) {
    dir.create(experiment_folder, recursive = TRUE)
  }

  experiment_table_path <- file.path(experiment_folder, "experiment_table.RDS")
  create_experiment_table_from_spec(
    spec,
    output_path = experiment_table_path,
    overwrite = overwrite,
    verbose = verbose
  )
  create_environments_from_spec(
    experiment_folder,
    spec,
    overwrite = overwrite,
    verbose = verbose
  )
  simulate_dynamics_from_spec(
    experiment_folder,
    spec,
    overwrite = overwrite,
    verbose = verbose
  )
  get_community_measures_from_spec(
    experiment_folder,
    spec,
    overwrite = overwrite,
    verbose = verbose
  )

  invisible(list(
    experiment_folder = experiment_folder,
    experiment_table = experiment_table_path,
    temperatures_db = file.path(experiment_folder, "temperatures.db"),
    simulation_summaries = file.path(experiment_folder, "simulation_summaries.RDS"),
    population_summaries = file.path(experiment_folder, "population_summaries.RDS"),
    community_measures = file.path(experiment_folder, "community_measures.RDS")
  ))
}
