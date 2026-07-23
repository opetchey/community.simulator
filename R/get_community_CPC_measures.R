# Get measures of community performance curves
#
# @param temperatures - temperature time series used in the simulations
# @param expt - experiment table with community parameters
# @param expt_def - experiment design information
# @param every_t - how often to sample the temperature time series (e.g., every 1 time step, every 10 time steps, etc.)
# @param soft_viability_scale Positive scale parameter for the soft viability
#   transform `plogis(g_i(T) / soft_viability_scale)`.
# @param parallel_community_measures Logical. If `TRUE`, calculate per-case
#   community performance curve measures in parallel where supported.
# @param parallel_workers Number of worker processes to use when
#   `parallel_community_measures` is `TRUE`.
# @param verbose Logical. If `TRUE`, print progress messages.
# @param progress_update_every Number of completed cases between progress
#   updates.
#
# @returns A dataset containing measures of community performance curves, including CV of community performance, synchrony of performance curves, and average CV of species performance curves weighted by their mean contribution to community performance.
# @keywords internal
#
# @examples NULL
get_community_CPC_measures <- function(temperatures,
                                 expt,
                                 expt_def,
                                 every_t = 1,
                                 soft_viability_scale = 0.01,
                                 parallel_community_measures = FALSE,
                                 parallel_workers = 1,
                                 verbose = TRUE,
                                 progress_update_every = 100) {



  # Open connections and read in data
  #conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  #temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  #expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  # Remove previous database if it exists
  #file.remove(paste0(experiment_folder, "imbalance.db"))
  #conn_imbalance <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "imbalance.db"))

  if (!is.numeric(soft_viability_scale) ||
      length(soft_viability_scale) != 1 ||
      !is.finite(soft_viability_scale) ||
      soft_viability_scale <= 0) {
    stop("`soft_viability_scale` must be a positive finite number.", call. = FALSE)
  }
  if (!is.numeric(every_t) || length(every_t) != 1 || is.na(every_t) || every_t < 1) {
    stop("`every_t` must be a positive number.", call. = FALSE)
  }
  parallel_workers <- as.integer(parallel_workers)
  if (is.na(parallel_workers)) {
    parallel_workers <- 1
  }
  parallel_workers <- max(1, min(parallel_workers, nrow(expt)))

  if (parallel_community_measures && parallel_workers > 1 && .Platform$OS.type == "windows") {
    warning(
      "`parallel_community_measures = TRUE` uses forked parallel workers, ",
      "which are not available on Windows. Falling back to serial CPC measures.",
      call. = FALSE
    )
    parallel_community_measures <- FALSE
  }

  temperatures_data <- temperatures |>
    dplyr::collect() |>
    dplyr::filter((.data$time %% every_t) == 0)
  temperatures_by_env <- split(temperatures_data, temperatures_data$env_series_id)

  calculate_species_performance <- function(comm_pars_i,
                                            case_id_oi,
                                            temperatures_oi) {
    model_type <- comm_pars_i$model_type
    if (is.null(model_type)) {
      model_type <- "lotka_volterra"
    }

    if (identical(model_type, "consumer_resource")) {
      calculate_consumer_resource_performance(comm_pars_i, case_id_oi, temperatures_oi)
    } else {
      calculate_lotka_volterra_performance(comm_pars_i, case_id_oi, temperatures_oi)
    }
  }

  calculate_lotka_volterra_performance <- function(comm_pars_i,
                                                   case_id_oi,
                                                   temperatures_oi) {
    species_pars <- tibble::tibble(
      case_id = rep(case_id_oi, length(comm_pars_i$birth_optimum_i)),
      species_id = paste0("Spp-", seq_along(comm_pars_i$birth_optimum_i)),
      birth_optimum = comm_pars_i$birth_optimum_i,
      birth_maximum = comm_pars_i$birth_maximum_i,
      birth_width = comm_pars_i$birth_width_i,
      death_intercept = comm_pars_i$death_intercept_i,
      death_temperature_slope = comm_pars_i$death_temperature_slope_i
    )

    species_pars1 <- species_pars |>
      dplyr::mutate(
        temperatures = list(temperatures_oi$temperature),
        time = list(temperatures_oi$time)
      ) |>
      tidyr::unnest(cols = c(temperatures, time))

    splt_spec <- split(species_pars1, species_pars1$species_id)

    species_pars2_naive <- lapply(splt_spec, function(df) {
      temp_range <- seq(
        min(species_pars1$temperatures),
        max(species_pars1$temperatures),
        length.out = length(df$temperatures)
      )
      df |>
        dplyr::mutate(
          growth_rate_naive = intrinsic_growth_gaussian(
            birth_maximum,
            birth_optimum,
            birth_width,
            death_intercept,
            death_temperature_slope,
            temp_range
          ),
          temp = temp_range
        )
    })

    species_pars2_naive <- do.call(rbind, species_pars2_naive)

    species_pars2_info <- lapply(splt_spec, function(df) {
      temp_range <- temperatures_oi$temperature
      df |>
        dplyr::mutate(
          growth_rate_info = intrinsic_growth_gaussian(
            birth_maximum,
            birth_optimum,
            birth_width,
            death_intercept,
            death_temperature_slope,
            temp_range
          ),
          temp = temp_range
        )
    })

    species_pars2_info <- do.call(rbind, species_pars2_info) |>
      dplyr::select(c("case_id", "species_id", "growth_rate_info", "time"))

    suppressMessages(
      dplyr::full_join(
        species_pars2_naive,
        species_pars2_info,
        by = c("case_id", "species_id", "time"),
        relationship = "many-to-many"
      )
    )
  }

  calculate_consumer_resource_performance <- function(comm_pars_i,
                                                      case_id_oi,
                                                      temperatures_oi) {
    species_ids <- paste0("Spp-", seq_len(comm_pars_i$S))

    consumer_growth <- function(temperature_values) {
      performance_matrix <- vapply(
        temperature_values,
        function(temperature) {
          uptake_maximum_ij <- comm_pars_i$uptake_maximum_ij *
            exp(-0.5 * ((temperature - comm_pars_i$uptake_optimum_ij) / comm_pars_i$uptake_width_ij)^2)
          comm_pars_i$e_i * rowSums(comm_pars_i$resource_use_ij * uptake_maximum_ij) -
            comm_pars_i$d_i
        },
        numeric(comm_pars_i$S)
      )

      tibble::tibble(
        species_id = rep(species_ids, times = length(temperature_values)),
        performance = as.vector(performance_matrix)
      )
    }

    info_performance <- consumer_growth(temperatures_oi$temperature) |>
      dplyr::mutate(
        case_id = case_id_oi,
        time = rep(temperatures_oi$time, each = comm_pars_i$S),
        growth_rate_info = .data$performance
      ) |>
      dplyr::select("case_id", "species_id", "time", "growth_rate_info")

    naive_temperatures <- seq(
      min(temperatures_oi$temperature),
      max(temperatures_oi$temperature),
      length.out = nrow(temperatures_oi)
    )
    naive_performance <- consumer_growth(naive_temperatures) |>
      dplyr::mutate(
        case_id = case_id_oi,
        time = rep(temperatures_oi$time, each = comm_pars_i$S),
        temperatures = rep(temperatures_oi$temperature, each = comm_pars_i$S),
        temp = rep(naive_temperatures, each = comm_pars_i$S),
        growth_rate_naive = .data$performance
      ) |>
      dplyr::select("case_id", "species_id", "temperatures", "time", "growth_rate_naive", "temp")

    suppressMessages(
      dplyr::full_join(
        naive_performance,
        info_performance,
        by = c("case_id", "species_id", "time"),
        relationship = "many-to-many"
      )
    )
  }

  calculate_case_cpc <- function(i) {
    case_id_oi <- expt$case_id[i]
    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures_by_env[[as.character(env_series_oi)]]
    if (is.null(temperatures_oi) || nrow(temperatures_oi) == 0) {
      stop(
        "No temperatures found for environment series `",
        env_series_oi,
        "` in case `",
        case_id_oi,
        "`.",
        call. = FALSE
      )
    }

    # Extract community parameters
    comm_pars_i <- expt$community_object[[i]]
    species_pars2 <- calculate_species_performance(
      comm_pars_i,
      case_id_oi,
      temperatures_oi
    ) |>
      dplyr::mutate(
        viability_binary_info = as.numeric(.data$growth_rate_info > 0),
        viability_binary_naive = as.numeric(.data$growth_rate_naive > 0),
        viability_soft_info = stats::plogis(.data$growth_rate_info / soft_viability_scale),
        viability_soft_naive = stats::plogis(.data$growth_rate_naive / soft_viability_scale)
    )


    tot_performance <- species_pars2 |>
      dplyr::group_by(case_id, temp) |>
      dplyr::summarise(community_growth_rate_info = sum(.data$growth_rate_info),
                       community_growth_rate_naive = sum(.data$growth_rate_naive),
                       community_viability_binary_info = sum(.data$viability_binary_info),
                       community_viability_binary_naive = sum(.data$viability_binary_naive),
                       community_viability_soft_info = sum(.data$viability_soft_info),
                       community_viability_soft_naive = sum(.data$viability_soft_naive),
                       .groups = "drop")

    # Calculate Loreau synchrony of performance curves

    tot_var <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(total_variance_growth_rate_info = stats::sd(community_growth_rate_info)^2,
                       total_variance_growth_rate_naive = stats::sd(community_growth_rate_naive)^2,.groups = "drop")



    species_sd <- species_pars2 |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::summarise(species_sd_growth_rate_info = stats::sd(growth_rate_info),
                       species_sd_growth_rate_naive = stats::sd(growth_rate_naive)) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(sum_species_sd_squared_growth_rate_info = (sum(species_sd_growth_rate_info))^2,
                       sum_species_sd_squared_growth_rate_naive = (sum(species_sd_growth_rate_naive))^2)

    synchrony <- suppressMessages(
      dplyr::full_join(species_sd, tot_var, by = "case_id")
    ) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(synchrony_performance_info = total_variance_growth_rate_info / sum_species_sd_squared_growth_rate_info,
                       synchrony_performance_naive = total_variance_growth_rate_naive / sum_species_sd_squared_growth_rate_naive,
                       .groups = "drop")


    # Get average CV

    tot_mean <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(total_mean_growth_rate_info = mean(community_growth_rate_info),
                       total_mean_growth_rate_naive = mean(community_growth_rate_naive), .groups = "drop")

    species_cv <- species_pars2 |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::summarise(mean_growth_rate_info = mean(growth_rate_info),
                       mean_growth_rate_naive = mean(growth_rate_naive),
                       sd_growth_rate_info = stats::sd(growth_rate_info),
                       sd_growth_rate_naive = stats::sd(growth_rate_naive)) |>
      dplyr::mutate(species_cv_growth_rate_info = sd_growth_rate_info / mean_growth_rate_info,
                    species_cv_growth_rate_naive = sd_growth_rate_naive / mean_growth_rate_naive)

    average_species_cv <- suppressMessages(
      dplyr::full_join(species_cv, tot_mean, by = "case_id")
    ) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(
        mean_species_cv_performance_info = sum((mean_growth_rate_info / total_mean_growth_rate_info) * species_cv_growth_rate_info),
        mean_species_cv_performance_naive = sum((mean_growth_rate_naive / total_mean_growth_rate_naive) * species_cv_growth_rate_naive),
        .groups = "drop"
      )







    species_pars4 <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(cv_community_performance_info = stats::sd(.data$community_growth_rate_info) / mean(.data$community_growth_rate_info),
                       cv_community_performance_naive = stats::sd(.data$community_growth_rate_naive) / mean(.data$community_growth_rate_naive),
                       cv_community_viability_binary_info = stats::sd(.data$community_viability_binary_info) / mean(.data$community_viability_binary_info),
                       cv_community_viability_binary_naive = stats::sd(.data$community_viability_binary_naive) / mean(.data$community_viability_binary_naive),
                       cv_community_viability_soft_info = stats::sd(.data$community_viability_soft_info) / mean(.data$community_viability_soft_info),
                       cv_community_viability_soft_naive = stats::sd(.data$community_viability_soft_naive) / mean(.data$community_viability_soft_naive),
                       .groups = "drop")


    species_pars4 <- suppressMessages(dplyr::full_join(species_pars4, synchrony))
    species_pars4 <- suppressMessages(dplyr::full_join(species_pars4, average_species_cv))

    species_pars4
  }

  case_indices <- seq_along(expt$case_id)
  report_cpc_progress <- make_progress_reporter(
    label = "Calculating CPC measures",
    total = length(case_indices),
    update_every = progress_update_every,
    enabled = verbose
  )

  if (parallel_community_measures && parallel_workers > 1) {
    if (verbose) {
      message("Calculating CPC measures in parallel with ", parallel_workers, " workers")
    }

    pending_cases <- case_indices
    active_jobs <- list()
    community_performance_measures <- vector("list", length(case_indices))
    completed_cases <- 0L

    start_job <- function(case_index) {
      job <- parallel::mcparallel(
        calculate_case_cpc(case_index),
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
            "CPC measure case ", case_index, " failed in a parallel worker: ",
            conditionMessage(attr(case_result, "condition")),
            call. = FALSE
          )
        }
        if (is.null(case_result)) {
          warning(
            "CPC measure case ",
            case_index,
            " did not return a result from a parallel worker; rerunning it in the main R process.",
            call. = FALSE
          )
          case_result <- calculate_case_cpc(case_index)
          if (is.null(case_result)) {
            stop(
              "CPC measure case ",
              case_index,
              " returned NULL after rerunning in the main R process.",
              call. = FALSE
            )
          }
        }

        community_performance_measures[[case_index]] <- case_result
        completed_cases <- completed_cases + 1L
        report_cpc_progress(completed_cases)
        active_jobs[[active_name]] <- NULL
      }
    }
  } else {
    community_performance_measures <- lapply(case_indices, function(i) {
      out <- calculate_case_cpc(i)
      report_cpc_progress(i)
      out
    })
  }

  #DBI::dbDisconnect(conn_imbalance)
  #DBI::dbDisconnect(conn_temperatures)

  # Combine and return
  final_measures <- dplyr::bind_rows(community_performance_measures)
  return(final_measures)
}
