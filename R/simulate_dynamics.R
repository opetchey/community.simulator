summarise_case_dynamics <- function(case_id,
                                    spts,
                                    returned_times,
                                    output_times,
                                    temperature_series,
                                    burn_in_duration,
                                    extinction_threshold = 1e-8) {
  consumer_data <- spts |>
    tibble::as_tibble() |>
    dplyr::mutate(time = returned_times) |>
    dplyr::filter(
      .data$time > burn_in_duration,
      .data$time %in% output_times
    )

  species_columns <- grep("^Spp", names(consumer_data), value = TRUE)
  abundance_matrix <- as.matrix(consumer_data[, species_columns, drop = FALSE])
  total_abundance <- rowSums(abundance_matrix, na.rm = TRUE)
  population_means <- colMeans(abundance_matrix, na.rm = TRUE)
  population_sds <- apply(abundance_matrix, 2, stats::sd, na.rm = TRUE)
  population_cvs <- population_sds / population_means
  final_abundances <- abundance_matrix[nrow(abundance_matrix), ]
  extinct_final <- final_abundances <= extinction_threshold
  extinct_ever <- apply(abundance_matrix <= extinction_threshold, 2, any, na.rm = TRUE)

  mean_totab <- mean(total_abundance, na.rm = TRUE)
  sd_totab <- stats::sd(total_abundance, na.rm = TRUE)
  sum_population_sds <- sum(population_sds, na.rm = TRUE)
  relative_abundances <- population_means / mean_totab

  temperature_data <- temperature_series |>
    dplyr::filter(
      .data$time > burn_in_duration,
      .data$time %in% output_times
    )
  temp_sensitivity <- NA_real_
  if (length(total_abundance) == nrow(temperature_data) &&
      length(total_abundance) >= 2 &&
      stats::var(temperature_data$temperature, na.rm = TRUE) > 0) {
    temp_model <- try(
      stats::lm(total_abundance ~ temperature, data = temperature_data),
      silent = TRUE
    )
    if (!inherits(temp_model, "try-error")) {
      temp_sensitivity <- unname(stats::coef(temp_model)[[2]])
    }
  }

  case_summary <- tibble::tibble(
    case_id = case_id,
    mean_totab = mean_totab,
    sd_totab = sd_totab,
    CV_totab = sd_totab / mean_totab,
    comm_temperature_sensitivity = temp_sensitivity,
    sync_ab = sd_totab^2 / sum_population_sds^2,
    pop_CV_ab = sum(relative_abundances * population_cvs, na.rm = TRUE),
    final_totab = total_abundance[[length(total_abundance)]],
    final_min_abundance = min(final_abundances, na.rm = TRUE),
    final_mean_abundance = mean(final_abundances, na.rm = TRUE),
    final_max_abundance = max(final_abundances, na.rm = TRUE),
    final_richness_above_extinction_threshold = sum(!extinct_final, na.rm = TRUE),
    n_extinct_final = sum(extinct_final, na.rm = TRUE),
    any_extinct_final = any(extinct_final, na.rm = TRUE),
    n_extinct_ever = sum(extinct_ever, na.rm = TRUE),
    any_extinct_ever = any(extinct_ever, na.rm = TRUE),
    min_abundance_ever = min(abundance_matrix, na.rm = TRUE),
    extinction_threshold = extinction_threshold
  )

  population_summary <- tibble::tibble(
    case_id = case_id,
    Species_ID = species_columns,
    mean_ab_pop = population_means,
    sd_pop = population_sds,
    CV_ab_pop = population_cvs,
    final_abundance = final_abundances,
    extinct_final = extinct_final,
    extinct_ever = extinct_ever
  )

  list(
    case_summary = case_summary,
    population_summary = population_summary
  )
}

format_simulation_duration <- function(seconds) {
  seconds <- as.numeric(seconds)
  if (!is.finite(seconds) || seconds < 0) {
    return("unknown")
  }
  if (seconds < 60) {
    return(paste0(round(seconds), " sec"))
  }

  total_minutes <- round(seconds / 60)
  days <- total_minutes %/% (24 * 60)
  hours <- (total_minutes %% (24 * 60)) %/% 60
  minutes <- total_minutes %% 60

  parts <- character()
  if (days > 0) {
    parts <- c(parts, paste0(days, " d"))
  }
  if (hours > 0 || days > 0) {
    parts <- c(parts, paste0(hours, " h"))
  }
  if (minutes > 0 || length(parts) == 0) {
    parts <- c(parts, paste0(minutes, " min"))
  }
  paste(parts, collapse = " ")
}

atomic_save_rds <- function(object, path) {
  temp_path <- tempfile(
    pattern = paste0(basename(path), "."),
    tmpdir = dirname(path),
    fileext = ".tmp"
  )
  on.exit({
    if (file.exists(temp_path)) {
      unlink(temp_path)
    }
  }, add = TRUE)

  saveRDS(object, temp_path)
  if (!file.rename(temp_path, path)) {
    if (file.exists(path)) {
      unlink(path)
    }
    if (!file.rename(temp_path, path)) {
      stop("Failed to write checkpoint file: ", path, call. = FALSE)
    }
  }
  invisible(path)
}

simulate_one_dynamics_case <- function(i,
                                       expt,
                                       temperatures_data,
                                       expt_def,
                                       dynamics_type,
                                       temperature_interpolation,
                                       immigration_rate,
                                       consumer_immigration_rate,
                                       initial_consumer_total_abundance,
                                       resource_initial_value,
                                       immigration_mode,
                                       ode_method,
                                       ode_rtol,
                                       ode_atol,
                                       ode_max_step,
                                       blowup_threshold,
                                       negative_tolerance,
                                       dynamics_save_every,
                                       resources_save_every,
                                       save_dynamics,
                                       save_resources,
                                       extinction_threshold,
                                       initial_abundance_seed_base) {

  env_series_oi <- expt$env_series_id[i]

  temperatures_oi <- temperatures_data |>
    dplyr::filter(.data$env_series_id == env_series_oi)

  burn_in_temps <- tibble::tibble(
    phase = rep("burn_in", expt_def$burn_in_duration),
    time = 1:expt_def$burn_in_duration,
    temperature = rep(
      expt$temperature_mean[i] %||% expt_def$temperature_mean,
      expt_def$burn_in_duration
    ),
    env_series_id = rep(env_series_oi, expt_def$burn_in_duration)
  )
  temperature_series <- dplyr::bind_rows(burn_in_temps, temperatures_oi)

  Tcel_control <- temperature_series$temperature
  Tcel_controlm <- matrix(Tcel_control, nrow = 1)
  integration_times <- seq_len(ncol(Tcel_controlm))
  output_times <- integration_times[
    ((integration_times - 1L) %% dynamics_save_every) == 0L |
      integration_times == max(integration_times)
  ]
  resource_output_times <- integration_times[
    ((integration_times - 1L) %% resources_save_every) == 0L |
      integration_times == max(integration_times)
  ]
  solver_output_times <- sort(unique(c(output_times, resource_output_times)))

  S <- expt[i, ]$community_object[[1]]$S
  if (!is.null(initial_abundance_seed_base)) {
    set.seed(initial_abundance_seed_base + i)
  }
  initial_abundances <- (dirmult::rdirichlet(1, rep(1, S)) *
                           initial_consumer_total_abundance)[1, ]

  resources_ts <- NULL
  returned_times <- integration_times
  if (dynamics_type == "discrete") {
    spts <- simulator_lv(
      input_com_params = expt$community_object[[i]],
      TcelSeries = Tcel_controlm,
      initial_abundances = initial_abundances
    )
  }

  if (dynamics_type == "continuous") {
    spts <- simulator_lv_continuous(
      input_com_params = expt$community_object[[i]],
      TcelSeries = Tcel_controlm,
      initial_abundances = initial_abundances,
      times = integration_times,
      output_times = output_times,
      temperature_interpolation = temperature_interpolation,
      immigration_rate = immigration_rate,
      immigration_mode = immigration_mode,
      ode_method = ode_method,
      rtol = ode_rtol,
      atol = ode_atol,
      max_step = ode_max_step,
      blowup_threshold = blowup_threshold
    )
    returned_times <- output_times
  }

  if (dynamics_type == "consumer_resource_continuous") {
    initial_resources <- rep(resource_initial_value, expt$community_object[[i]]$R)
    cr_output <- simulator_consumer_resource_continuous(
      input_com_params = expt$community_object[[i]],
      TcelSeries = Tcel_controlm,
      initial_consumer_abundances = initial_abundances,
      initial_resource_values = initial_resources,
      times = integration_times,
      output_times = solver_output_times,
      temperature_interpolation = temperature_interpolation,
      consumer_immigration_rate = consumer_immigration_rate,
      ode_method = ode_method,
      rtol = ode_rtol,
      atol = ode_atol,
      max_step = ode_max_step,
      blowup_threshold = blowup_threshold,
      negative_tolerance = negative_tolerance
    )
    spts <- cr_output$consumers
    resources_ts <- cr_output$resources
    returned_times <- solver_output_times
  }

  online_summaries <- summarise_case_dynamics(
    case_id = expt$case_id[i],
    spts = spts,
    returned_times = returned_times,
    output_times = output_times,
    temperature_series = temperature_series,
    burn_in_duration = expt_def$burn_in_duration,
    extinction_threshold = extinction_threshold
  )

  if (save_dynamics) {
    spts <- spts |>
      tibble::as_tibble() |>
      dplyr::mutate(
        case_id = expt$case_id[i],
        time = returned_times
      ) |>
      tidyr::pivot_longer(
        names_to = "Species_ID",
        values_to = "Abundance",
        cols = dplyr::starts_with("Spp")
      ) |>
      dplyr::filter(
        .data$time > expt_def$burn_in_duration,
        .data$time %in% output_times
      )
  } else {
    spts <- NULL
  }

  if (save_resources && !is.null(resources_ts)) {
    resources_ts <- resources_ts |>
      tibble::as_tibble() |>
      dplyr::mutate(
        case_id = expt$case_id[i],
        time = returned_times
      ) |>
      tidyr::pivot_longer(
        names_to = "Resource_ID",
        values_to = "Resource",
        cols = dplyr::starts_with("Res")
      ) |>
      dplyr::filter(
        .data$time > expt_def$burn_in_duration,
        .data$time %in% resource_output_times
      )
  } else {
    resources_ts <- NULL
  }

  list(
    dynamics = spts,
    resources = resources_ts,
    case_summary = online_summaries$case_summary,
    population_summary = online_summaries$population_summary
  )
}

#' Legacy JSON dynamics simulator
#'
#' This internal helper simulates cases for the old JSON experiment format. New
#' user-facing workflows should use [simulate_dynamics_from_spec()] through
#' [run_experiment()].
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing dynamics database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @details Old experiment JSON files can set `model_type` to `"lv_discrete"`,
#'   `"lv_continuous"`, or `"consumer_resource_continuous"`. They can also
#'   optionally include
#'   `parallel_simulations`, `parallel_workers`, and
#'   `initial_abundance_seed_base`. They can also include output-control options:
#'   `save_dynamics`, `save_resources`, `dynamics_save_every`, and
#'   `resources_save_every`. They can also include `summary_checkpoint_every`
#'   and `runtime_update_every`. The save interval values are integers giving
#'   the interval between saved output time points. `resources_save_every`
#'   defaults to `dynamics_save_every`. Compact case and population summaries
#'   are checkpointed by the parent process every `summary_checkpoint_every`
#'   completed cases and written again at the end. `save_dynamics` and
#'   `save_resources` control only the diagnostic SQLite outputs.
#'   `save_resources` only applies to
#'   consumer-resource dynamics. When `parallel_simulations` evaluates to
#'   `TRUE`, simulation cases are computed in parallel and SQLite tables are
#'   still written serially by the parent process. Parallel processing is
#'   intended for macOS/Linux.
#'
#' @return Returns nothing. Always saves compact `simulation_summaries.RDS` and
#'   `population_summaries.RDS`; optionally saves SQLite dynamics/resources
#'   databases depending on output-control settings.
#' @keywords internal
#'
#' @examples NULL
simulate_dynamics <- function(experiment_folder,
                              experiment_design_filename,
                              overwrite = FALSE,
                              verbose = TRUE) {

  require_dbplyr()

  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  on.exit(DBI::dbDisconnect(conn_temperatures), add = TRUE)
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  temperatures_data <- temperatures |>
    dplyr::collect()

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  get_design_value <- function(name, default, aliases = character()) {
    candidates <- c(name, aliases)
    selected <- candidates[vapply(candidates, function(candidate) {
      !is.null(expt_def[[candidate]]) && length(expt_def[[candidate]]) > 0
    }, logical(1))]
    if (length(selected) == 0) {
      return(default)
    }

    value <- expt_def[[selected[[1]]]]
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

  dynamics_type <- normalize_model_type(get_design_value(
    "model_type",
    "lv_discrete",
    aliases = "dynamics_type"
  ))
  temperature_interpolation <- get_design_value("temperature_interpolation", "linear")
  immigration_rate <- get_design_value("immigration_rate", 0.1)
  consumer_immigration_rate <- get_design_value("consumer_immigration_rate", immigration_rate)
  initial_consumer_total_abundance <- get_design_value("initial_consumer_total_abundance", 1000)
  resource_initial_value <- get_design_value("resource_initial_value", 100)
  immigration_mode <- get_design_value("immigration_mode", "continuous")
  ode_method <- get_design_value("ode_method", "lsoda")
  ode_rtol <- get_design_value("ode_rtol", 1e-6)
  ode_atol <- get_design_value("ode_atol", 1e-8)
  ode_max_step <- get_design_value("ode_max_step", 1)
  blowup_threshold <- get_design_value("blowup_threshold", 1e12)
  negative_tolerance <- get_design_value("negative_tolerance", 1e-8)
  dynamics_save_every <- as.integer(get_design_value("dynamics_save_every", 1))
  if (is.na(dynamics_save_every) || dynamics_save_every < 1) {
    stop("`dynamics_save_every` must evaluate to an integer >= 1.", call. = FALSE)
  }
  resources_save_every <- as.integer(get_design_value("resources_save_every", dynamics_save_every))
  if (is.na(resources_save_every) || resources_save_every < 1) {
    stop("`resources_save_every` must evaluate to an integer >= 1.", call. = FALSE)
  }
  save_dynamics <- isTRUE(get_design_value("save_dynamics", TRUE))
  save_resources <- isTRUE(get_design_value("save_resources", TRUE)) &&
    dynamics_type == "consumer_resource_continuous"
  extinction_threshold <- as.numeric(get_design_value("extinction_threshold", 1e-8))
  if (is.na(extinction_threshold) || extinction_threshold < 0) {
    stop("`extinction_threshold` must evaluate to a non-negative number.", call. = FALSE)
  }
  summary_checkpoint_every <- as.integer(get_design_value("summary_checkpoint_every", 1))
  if (is.na(summary_checkpoint_every) || summary_checkpoint_every < 1) {
    stop("`summary_checkpoint_every` must evaluate to an integer >= 1.", call. = FALSE)
  }
  runtime_update_every <- as.integer(get_design_value(
    "runtime_update_every",
    max(1L, floor(nrow(expt) / 100))
  ))
  if (is.na(runtime_update_every) || runtime_update_every < 1) {
    stop("`runtime_update_every` must evaluate to an integer >= 1.", call. = FALSE)
  }
  simulation_progress <- isTRUE(get_design_value("simulation_progress", verbose))
  parallel_simulations <- isTRUE(get_design_value("parallel_simulations", FALSE))
  parallel_workers <- as.integer(get_design_value(
    "parallel_workers",
    max(1, parallel::detectCores(logical = FALSE) - 1)
  ))
  if (is.na(parallel_workers)) {
    parallel_workers <- 1
  }
  parallel_workers <- max(1, min(parallel_workers, nrow(expt)))
  initial_abundance_seed_base <- get_design_value("initial_abundance_seed_base", NULL)
  if (!is.null(initial_abundance_seed_base)) {
    initial_abundance_seed_base <- as.integer(initial_abundance_seed_base)
  }
  initial_abundance_seed_metadata <- if (is.null(initial_abundance_seed_base)) {
    NA_character_
  } else {
    as.character(initial_abundance_seed_base)
  }

  if (!dynamics_type %in% c("discrete", "continuous", "consumer_resource_continuous")) {
    stop(
      "`model_type` must be 'lv_discrete', 'lv_continuous', or ",
      "'consumer_resource_continuous'.",
      call. = FALSE
    )
  }

  if (!save_dynamics && !save_resources && verbose) {
    warning(
      "Both `save_dynamics` and `save_resources` are FALSE. ",
      "Dynamics will be simulated and compact summaries written, but no ",
      "dynamics/resources database will be written.",
      call. = FALSE
    )
  }

  summaries_path <- paste0(experiment_folder, "simulation_summaries.RDS")
  population_summaries_path <- paste0(experiment_folder, "population_summaries.RDS")
  prepare_output_path(summaries_path, overwrite = overwrite, verbose = verbose, label = "simulation summaries file")
  prepare_output_path(population_summaries_path, overwrite = overwrite, verbose = verbose, label = "population summaries file")

  conn_dynamics <- NULL
  output_path <- paste0(experiment_folder, "dynamics.db")
  if (save_dynamics) {
    prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "dynamics database")
    conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  } else if (overwrite && file.exists(output_path)) {
    prepare_output_path(output_path, overwrite = TRUE, verbose = verbose, label = "disabled dynamics database")
  }

  conn_resources <- NULL
  resources_output_path <- paste0(experiment_folder, "resources.db")
  if (save_resources) {
    resources_output_path <- paste0(experiment_folder, "resources.db")
    prepare_output_path(resources_output_path, overwrite = overwrite, verbose = verbose, label = "resources database")
    conn_resources <- DBI::dbConnect(RSQLite::SQLite(), resources_output_path)
  } else if (overwrite && file.exists(resources_output_path)) {
    prepare_output_path(resources_output_path, overwrite = TRUE, verbose = verbose, label = "disabled resources database")
  }

  dynamics_metadata <- tibble::tibble(
    key = c(
      "dynamics_type",
      "temperature_interpolation",
      "immigration_rate",
      "consumer_immigration_rate",
      "initial_consumer_total_abundance",
      "resource_initial_value",
      "immigration_mode",
      "ode_method",
      "ode_rtol",
      "ode_atol",
      "ode_max_step",
      "blowup_threshold",
      "negative_tolerance",
      "dynamics_save_every",
      "resources_save_every",
      "save_dynamics",
      "save_resources",
      "extinction_threshold",
      "summary_checkpoint_every",
      "runtime_update_every",
      "simulation_progress",
      "initial_abundance_seed_base",
      "parallel_simulations",
      "parallel_workers"
    ),
    value = as.character(c(
      dynamics_type,
      temperature_interpolation,
      immigration_rate,
      consumer_immigration_rate,
      initial_consumer_total_abundance,
      resource_initial_value,
      immigration_mode,
      ode_method,
      ode_rtol,
      ode_atol,
      ode_max_step,
      blowup_threshold,
      negative_tolerance,
      dynamics_save_every,
      resources_save_every,
      save_dynamics,
      save_resources,
      extinction_threshold,
      summary_checkpoint_every,
      runtime_update_every,
      simulation_progress,
      initial_abundance_seed_metadata,
      parallel_simulations,
      parallel_workers
    ))
  )
  if (!is.null(conn_dynamics)) {
    DBI::dbWriteTable(conn_dynamics, "dynamics_metadata", dynamics_metadata, overwrite = TRUE)
  }
  if (!is.null(conn_resources)) {
    DBI::dbWriteTable(conn_resources, "resources_metadata", dynamics_metadata, overwrite = TRUE)
  }


  case_indices <- seq_len(nrow(expt))
  simulate_case <- function(i) {
    if (verbose && !simulation_progress) {
      message("Simulating case ", i, " of ", nrow(expt))
    }
    simulate_one_dynamics_case(
      i = i,
      expt = expt,
      temperatures_data = temperatures_data,
      expt_def = expt_def,
      dynamics_type = dynamics_type,
      temperature_interpolation = temperature_interpolation,
      immigration_rate = immigration_rate,
      consumer_immigration_rate = consumer_immigration_rate,
      initial_consumer_total_abundance = initial_consumer_total_abundance,
      resource_initial_value = resource_initial_value,
      immigration_mode = immigration_mode,
      ode_method = ode_method,
      ode_rtol = ode_rtol,
      ode_atol = ode_atol,
      ode_max_step = ode_max_step,
      blowup_threshold = blowup_threshold,
      negative_tolerance = negative_tolerance,
      dynamics_save_every = dynamics_save_every,
      resources_save_every = resources_save_every,
      save_dynamics = save_dynamics,
      save_resources = save_resources,
      extinction_threshold = extinction_threshold,
      initial_abundance_seed_base = initial_abundance_seed_base
    )
  }

  progress_bar <- NULL
  completed_cases <- 0L
  simulation_start_time <- Sys.time()
  last_summary_checkpoint_completed <- 0L
  last_runtime_update_completed <- 0L
  if (simulation_progress) {
    progress_bar <- utils::txtProgressBar(
      min = 0,
      max = length(case_indices),
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
    completed_cases <<- completed_cases + n
    if (!is.null(progress_bar)) {
      utils::setTxtProgressBar(progress_bar, completed_cases)
    }
  }

  dynamics_table_written <- FALSE
  resources_table_written <- FALSE
  case_summaries <- vector("list", length(case_indices))
  population_summaries <- vector("list", length(case_indices))

  write_case_result <- function(case_result, case_index) {
    case_summaries[[case_index]] <<- case_result$case_summary
    population_summaries[[case_index]] <<- case_result$population_summary

    if (!is.null(conn_dynamics) && !is.null(case_result$dynamics)) {
      DBI::dbWriteTable(
        conn_dynamics,
        "dynamics",
        case_result$dynamics,
        overwrite = !dynamics_table_written,
        append = dynamics_table_written
      )
      dynamics_table_written <<- TRUE
    }

    if (!is.null(conn_resources) && !is.null(case_result$resources)) {
      DBI::dbWriteTable(
        conn_resources,
        "resources",
        case_result$resources,
        overwrite = !resources_table_written,
        append = resources_table_written
      )
      resources_table_written <<- TRUE
    }
  }

  checkpoint_summary_outputs <- function(force = FALSE) {
    if (completed_cases == 0L) {
      return(invisible(FALSE))
    }
    if (!force &&
        completed_cases - last_summary_checkpoint_completed < summary_checkpoint_every) {
      return(invisible(FALSE))
    }

    atomic_save_rds(dplyr::bind_rows(case_summaries), summaries_path)
    atomic_save_rds(dplyr::bind_rows(population_summaries), population_summaries_path)
    last_summary_checkpoint_completed <<- completed_cases

    invisible(TRUE)
  }

  report_runtime_estimate <- function(force = FALSE) {
    if (!verbose || completed_cases == 0L) {
      return(invisible(FALSE))
    }
    if (force && last_runtime_update_completed == completed_cases) {
      return(invisible(FALSE))
    }
    if (!force &&
        completed_cases - last_runtime_update_completed < runtime_update_every &&
        completed_cases < length(case_indices)) {
      return(invisible(FALSE))
    }

    elapsed_seconds <- as.numeric(difftime(Sys.time(), simulation_start_time, units = "secs"))
    seconds_per_case <- elapsed_seconds / completed_cases
    remaining_cases <- length(case_indices) - completed_cases
    remaining_seconds <- seconds_per_case * remaining_cases

    if (!is.null(progress_bar)) {
      cat("\n")
    }
    message(
      "Runtime: ",
      completed_cases,
      "/",
      length(case_indices),
      " cases complete; elapsed ",
      format_simulation_duration(elapsed_seconds),
      "; estimated remaining ",
      format_simulation_duration(remaining_seconds)
    )
    last_runtime_update_completed <<- completed_cases
    invisible(TRUE)
  }

  record_case_completion <- function(case_result, case_index) {
    write_case_result(case_result, case_index)
    update_progress()
    checkpoint_summary_outputs()
    report_runtime_estimate()
  }

  if (parallel_simulations && parallel_workers > 1) {
    if (verbose) {
      message("Simulating dynamics in parallel with ", parallel_workers, " workers")
    }

    pending_cases <- case_indices
    active_jobs <- list()

    start_job <- function(case_index) {
      job <- parallel::mcparallel(
        simulate_case(case_index),
        name = as.character(case_index),
        silent = TRUE
      )
      list(case_index = case_index, job = job)
    }

    while (length(pending_cases) > 0 || length(active_jobs) > 0) {
      while (length(pending_cases) > 0 && length(active_jobs) < parallel_workers) {
        next_case <- pending_cases[[1]]
        pending_cases <- pending_cases[-1]
        active_jobs[[as.character(next_case)]] <- start_job(next_case)
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
        case_index <- active_jobs[[active_name]]$case_index
        case_result <- collected[[pid]]
        if (inherits(case_result, "try-error")) {
          stop(
            "Simulation case ", case_index, " failed in a parallel worker: ",
            conditionMessage(attr(case_result, "condition")),
            call. = FALSE
          )
        }
        record_case_completion(case_result, case_index)
        active_jobs[[active_name]] <- NULL
      }
    }
  } else {
    for (i in case_indices) {
      case_result <- simulate_case(i)
      record_case_completion(case_result, i)
    }
  }

  if (!is.null(progress_bar)) {
    close(progress_bar)
    progress_bar <- NULL
  }

  if (!is.null(conn_resources)) {
    DBI::dbDisconnect(conn_resources)
    conn_resources <- NULL
    announce_output_written(resources_output_path, verbose = verbose, label = "resources database")
  }
  if (!is.null(conn_dynamics)) {
    DBI::dbDisconnect(conn_dynamics)
    conn_dynamics <- NULL
    announce_output_written(output_path, verbose = verbose, label = "dynamics database")
  }

  checkpoint_summary_outputs(force = TRUE)
  report_runtime_estimate(force = TRUE)
  announce_output_written(summaries_path, verbose = verbose, label = "simulation summaries file")
  announce_output_written(population_summaries_path, verbose = verbose, label = "population summaries file")

}
