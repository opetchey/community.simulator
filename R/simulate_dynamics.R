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
                                       initial_abundance_seed_base) {

  env_series_oi <- expt$env_series_id[i]

  temperatures_oi <- temperatures_data |>
    dplyr::filter(.data$env_series_id == env_series_oi)

  burn_in_temps <- tibble::tibble(
    phase = rep("burn_in", expt_def$burn_in_duration),
    time = 1:expt_def$burn_in_duration,
    temperature = rep(expt_def$temperature_mean, expt_def$burn_in_duration),
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
      output_times = output_times,
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
    returned_times <- output_times
  }

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

  if (!is.null(resources_ts)) {
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
        .data$time %in% output_times
      )
  }

  list(
    dynamics = spts,
    resources = resources_ts
  )
}

#' Simulate the dynamics of all the cases in an experiment
#' Unfortunately at the moment has features of temperature series hard coded in
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing dynamics database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @details Experiment JSON files can optionally include
#'   `parallel_simulations`, `parallel_workers`, and
#'   `initial_abundance_seed_base`. They can also include
#'   `dynamics_save_every`, an integer giving the interval between saved
#'   consumer/resource output time points. When `parallel_simulations`
#'   evaluates to `TRUE`, simulation cases are computed in parallel and SQLite
#'   tables are still written serially by the parent process. Parallel
#'   processing is intended for macOS/Linux.
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series and the community dynamics.
#' @export
#'
#' @examples NULL
simulate_dynamics <- function(experiment_folder,
                              experiment_design_filename,
                              overwrite = FALSE,
                              verbose = TRUE) {

  require_dbplyr()

  ## setup the databases for saving the temperature time series and the community dynamics
  ## set up data base to save results into
  output_path <- paste0(experiment_folder, "dynamics.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "dynamics database")
  conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  temperatures_data <- temperatures |>
    dplyr::collect()

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

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

  dynamics_type <- get_design_value("dynamics_type", "discrete")
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
      "`dynamics_type` must be 'discrete', 'continuous', or 'consumer_resource_continuous'.",
      call. = FALSE
    )
  }

  conn_resources <- NULL
  if (dynamics_type == "consumer_resource_continuous") {
    resources_output_path <- paste0(experiment_folder, "resources.db")
    prepare_output_path(resources_output_path, overwrite = overwrite, verbose = verbose, label = "resources database")
    conn_resources <- DBI::dbConnect(RSQLite::SQLite(), resources_output_path)
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
      simulation_progress,
      initial_abundance_seed_metadata,
      parallel_simulations,
      parallel_workers
    ))
  )
  DBI::dbWriteTable(conn_dynamics, "dynamics_metadata", dynamics_metadata, overwrite = TRUE)
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
      initial_abundance_seed_base = initial_abundance_seed_base
    )
  }

  progress_bar <- NULL
  completed_cases <- 0L
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

  write_case_result <- function(case_result) {
    DBI::dbWriteTable(
      conn_dynamics,
      "dynamics",
      case_result$dynamics,
      overwrite = !dynamics_table_written,
      append = dynamics_table_written
    )
    dynamics_table_written <<- TRUE

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
        write_case_result(case_result)
        active_jobs[[active_name]] <- NULL
        update_progress()
      }
    }
  } else {
    for (i in case_indices) {
      case_result <- simulate_case(i)
      write_case_result(case_result)
      update_progress()
    }
  }

  if (!is.null(progress_bar)) {
    close(progress_bar)
    progress_bar <- NULL
  }

  DBI::dbDisconnect(conn_temperatures)
  if (!is.null(conn_resources)) {
    DBI::dbDisconnect(conn_resources)
    announce_output_written(resources_output_path, verbose = verbose, label = "resources database")
  }
  DBI::dbDisconnect(conn_dynamics)
  announce_output_written(output_path, verbose = verbose, label = "dynamics database")

}
