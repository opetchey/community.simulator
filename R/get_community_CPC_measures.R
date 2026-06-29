#' Get measures of community performance curves
#'
#' @param temperatures - temperature time series used in the simulations
#' @param expt - experiment table with community parameters
#' @param expt_def - experiment design information
#' @param every_t - how often to sample the temperature time series (e.g., every 1 time step, every 10 time steps, etc.)
#' @param soft_viability_scale Positive scale parameter for the soft viability
#'   transform `plogis(g_i(T) / soft_viability_scale)`.
#'
#' @returns A dataset containing measures of community performance curves, including CV of community performance, synchrony of performance curves, and average CV of species performance curves weighted by their mean contribution to community performance.
#' @export
#'
#' @examples NULL
get_community_CPC_measures <- function(temperatures,
                                 expt,
                                 expt_def,
                                 every_t = 1,
                                 soft_viability_scale = 0.01) {



  # Open connections and read in data
  #conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  #temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  #expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  #expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  # Remove previous database if it exists
  #file.remove(paste0(experiment_folder, "imbalance.db"))
  #conn_imbalance <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "imbalance.db"))



  # Initialize list to collect results
  all_CV_community_perf <- list()

  if (!is.numeric(soft_viability_scale) ||
      length(soft_viability_scale) != 1 ||
      !is.finite(soft_viability_scale) ||
      soft_viability_scale <= 0) {
    stop("`soft_viability_scale` must be a positive finite number.", call. = FALSE)
  }

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
      case_id = rep(case_id_oi, length(comm_pars_i$b_opt_i)),
      species_id = paste0("Spp-", seq_along(comm_pars_i$b_opt_i)),
      b_opt_i = comm_pars_i$b_opt_i,
      a_b_i = comm_pars_i$a_b_i,
      s_i = comm_pars_i$s_i,
      a_d_i = comm_pars_i$a_d_i,
      z_i = comm_pars_i$z_i
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
          igr_naive = intrinsic_growth_gaussian(
            a_b_i, b_opt_i, s_i, a_d_i, z_i, temp_range
          ),
          temp = temp_range
        )
    })

    species_pars2_naive <- do.call(rbind, species_pars2_naive)

    species_pars2_info <- lapply(splt_spec, function(df) {
      temp_range <- temperatures_oi$temperature
      df |>
        dplyr::mutate(
          igr_info = intrinsic_growth_gaussian(
            a_b_i, b_opt_i, s_i, a_d_i, z_i, temp_range
          ),
          temp = temp_range
        )
    })

    species_pars2_info <- do.call(rbind, species_pars2_info) |>
      dplyr::select(c("case_id", "species_id", "igr_info", "time"))

    suppressMessages(
      dplyr::full_join(species_pars2_naive, species_pars2_info)
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
          u_max_ij <- comm_pars_i$a_u_ij *
            exp(-0.5 * ((temperature - comm_pars_i$u_opt_ij) / comm_pars_i$sd_u_ij)^2)
          comm_pars_i$e_i * rowSums(comm_pars_i$resource_use_ij * u_max_ij) -
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
        igr_info = .data$performance
      ) |>
      dplyr::select("case_id", "species_id", "time", "igr_info")

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
        igr_naive = .data$performance
      ) |>
      dplyr::select("case_id", "species_id", "temperatures", "time", "igr_naive", "temp")

    suppressMessages(
      dplyr::full_join(naive_performance, info_performance)
    )
  }

  # Loop through each case
  for (i in seq_along(expt$case_id)) {

    if(i %% 100 == 0){
      print(paste0("Processing CPC case: ", i))
    }


    case_id_oi <- expt$case_id[i]
    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      dplyr::filter(env_series_id == env_series_oi) |>
      dplyr::collect() |>
      dplyr::filter((time %% every_t) == 0)

    # Extract community parameters
    comm_pars_i <- expt$community_object[[i]]
    species_pars2 <- calculate_species_performance(
      comm_pars_i,
      case_id_oi,
      temperatures_oi
    ) |>
      dplyr::mutate(
        viability_binary_info = as.numeric(.data$igr_info > 0),
        viability_binary_naive = as.numeric(.data$igr_naive > 0),
        viability_soft_info = stats::plogis(.data$igr_info / soft_viability_scale),
        viability_soft_naive = stats::plogis(.data$igr_naive / soft_viability_scale)
    )


    tot_performance <- species_pars2 |>
      dplyr::group_by(case_id, temp) |>
      dplyr::summarise(community_igr_info = sum(.data$igr_info),
                       community_igr_naive = sum(.data$igr_naive),
                       community_viability_binary_info = sum(.data$viability_binary_info),
                       community_viability_binary_naive = sum(.data$viability_binary_naive),
                       community_viability_soft_info = sum(.data$viability_soft_info),
                       community_viability_soft_naive = sum(.data$viability_soft_naive),
                       .groups = "drop")

    # Calculate Loreau synchrony of performance curves

    tot_var <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(tot_var_info= stats::sd(community_igr_info)^2,
                       tot_var_naive= stats::sd(community_igr_naive)^2,.groups = "drop")



    species_sd <- species_pars2 |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::summarise(spec_sd_info = stats::sd(igr_info),
                       spec_sd_naive = stats::sd(igr_naive)) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(sum_sd_2_info = (sum(spec_sd_info))^2,
                       sum_sd_2_naive = (sum(spec_sd_naive))^2)

    synchrony <- suppressMessages(
      dplyr::full_join(species_sd, tot_var, by = "case_id")
    ) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(synchrony_perf_info = tot_var_info / sum_sd_2_info,
                       synchrony_perf_naive = tot_var_naive / sum_sd_2_naive,
                       .groups = "drop")


    # Get average CV

    tot_mean <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(tot_mean_info = mean(community_igr_info),
                       tot_mean_naive = mean(community_igr_naive), .groups = "drop")

    spec_CV <- species_pars2 |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::summarise(mean_igr_info = mean(igr_info),
                       mean_igr_naive = mean(igr_naive),
                       sd_igr_info = stats::sd(igr_info),
                       sd_igr_naive = stats::sd(igr_naive)) |>
      dplyr::mutate(spec_CV_info = sd_igr_info / mean_igr_info,
                    spec_CV_naive = sd_igr_naive / mean_igr_naive)

    avg_spc_CV <- suppressMessages(
      dplyr::full_join(spec_CV, tot_mean, by = "case_id")
    ) |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(
        avg_perf_CV_info = sum((mean_igr_info / tot_mean_info) * spec_CV_info),
        avg_perf_CV_naive = sum((mean_igr_naive / tot_mean_naive) * spec_CV_naive),
        .groups = "drop"
      )







    species_pars4 <- tot_performance |>
      dplyr::group_by(case_id) |>
      dplyr::summarise(CV_community_perf_info = stats::sd(.data$community_igr_info) / mean(.data$community_igr_info),
                       CV_community_perf_naive = stats::sd(.data$community_igr_naive) / mean(.data$community_igr_naive),
                       CV_community_viability_binary_info = stats::sd(.data$community_viability_binary_info) / mean(.data$community_viability_binary_info),
                       CV_community_viability_binary_naive = stats::sd(.data$community_viability_binary_naive) / mean(.data$community_viability_binary_naive),
                       CV_community_viability_soft_info = stats::sd(.data$community_viability_soft_info) / mean(.data$community_viability_soft_info),
                       CV_community_viability_soft_naive = stats::sd(.data$community_viability_soft_naive) / mean(.data$community_viability_soft_naive),
                       .groups = "drop")


    species_pars4 <- suppressMessages(dplyr::full_join(species_pars4, synchrony))
    species_pars4 <- suppressMessages(dplyr::full_join(species_pars4, avg_spc_CV))

    all_CV_community_perf[[i]] <- species_pars4



    # if (i == 1) {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, overwrite = TRUE)
    # } else {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, append = TRUE)
    # }
  }

  #DBI::dbDisconnect(conn_imbalance)
  #DBI::dbDisconnect(conn_temperatures)

  # Combine and return
  final_CV <- dplyr::bind_rows(all_CV_community_perf)
  return(final_CV)
}
