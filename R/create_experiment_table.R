#' This function takes the experimental design from the specified JSON experiment design file and creates a table of all the simulations that will be run.
#'
#' @param experiment_folder Folder where the experiment data will be saved
#' @param experiment_design_filename Name of the experiment definition file
#' @param overwrite Logical. If `TRUE`, overwrite an existing experiment table.
#' @param verbose Logical. If `TRUE`, print messages about written outputs.
#'
#' @return Returns the number of cases in the experiment. Also saves to RDS the experiment design, for later use.
#' @importFrom rlang .data
#' @export
#'
#' @examples NULL
create_experiment_table <- function(experiment_folder,
                                    experiment_design_filename,
                                    overwrite = FALSE,
                                    verbose = TRUE) {

  expt_def <- read_experiment_design_json(experiment_folder, experiment_design_filename)
  output_path <- paste0(experiment_folder, "experiment_table.RDS")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "experiment table")

  #if(eval(expt_def$number) == "regular" &
  #   eval(expt_def$a_b_number_of_realisations) > 1) {
  #  stop("Regular distribution of a_b values cannot have more than one realisation")
  #}

  #if(eval(expt_def$b_opt_distribution) == "regular" &
  #   eval(expt_def$b_opt_number_of_realisations) > 1) {
  #  stop("Regular distribution of b_opt values cannot have more than one realisation")
  #}


  set.seed(eval(expt_def$random_seed))

  eval_or_default <- function(name, default) {
    value <- expt_def[[name]]
    if (is.null(value)) {
      return(default)
    }
    eval(value)
  }

  dynamics_type <- eval_or_default("dynamics_type", "discrete")

  number_of_species <- expt_def$number_of_species_treatment
  if (is.null(number_of_species)) {
    number_of_species <- expt_def$number_of_species
  }

  if (dynamics_type == "consumer_resource_continuous") {
    expt <- expand.grid(
      u_max_mean = eval(expt_def$u_max_mean_treatment),
      u_max_range = eval(expt_def$u_max_range_treatment),
      u_max_distribution = eval(expt_def$u_max_distribution),

      u_opt_mean = eval(expt_def$u_opt_mean_treatment),
      u_opt_range = eval(expt_def$u_opt_range_treatment),
      u_opt_distribution = eval(expt_def$u_opt_distribution),

      sd_u_distribution = eval(expt_def$sd_u_distribution),
      sd_u_mean = eval(expt_def$sd_u_mean),
      sd_u_range = eval(expt_def$sd_u_range),

      half_saturation_mean = eval(expt_def$half_saturation_mean_treatment),
      half_saturation_range = eval(expt_def$half_saturation_range_treatment),
      half_saturation_distribution = eval(expt_def$half_saturation_distribution),

      consumer_death_rate = eval(expt_def$consumer_death_rate_treatment),
      resource_renewal_rate = eval(expt_def$resource_renewal_rate_treatment),
      resource_supply = eval(expt_def$resource_supply_treatment),
      conversion_efficiency = eval(expt_def$conversion_efficiency),
      consumer_immigration_rate = eval(expt_def$consumer_immigration_rate),
      resource_use_mode = eval(expt_def$resource_use_mode),
      active_resource = eval_or_default("active_resource", 1),
      resource_specialization = eval_or_default("resource_specialization", 1),

      community_replicate = 1:eval(expt_def$number_of_community_replicates),

      temperature_mean = eval(expt_def$temperature_mean),
      temperature_sd = eval(expt_def$temperature_sd),
      one_over_f_gamma = eval(expt_def$one_over_f_gamma),

      temperature_replicate = 1:eval(expt_def$number_of_environment_replicates),

      richness = eval(number_of_species)
    ) |>
      dplyr::mutate(
        env_series_id = paste0("env_series_", temperature_mean, "_",
                               temperature_sd, "_",
                               one_over_f_gamma, "_",
                               temperature_replicate),
        community_id = paste0("cr_community_", .data$u_max_mean, "_",
                              .data$u_max_range, "_",
                              .data$u_opt_mean, "_",
                              .data$u_opt_range, "_",
                              .data$sd_u_mean, "_",
                              .data$sd_u_range, "_",
                              .data$half_saturation_mean, "_",
                              .data$half_saturation_range, "_",
                              .data$consumer_death_rate, "_",
                              .data$resource_renewal_rate, "_",
                              .data$resource_supply, "_",
                              .data$resource_use_mode, "_",
                              .data$active_resource, "_",
                              .data$resource_specialization, "_",
                              .data$community_replicate, "_",
                              .data$richness),
        case_id = paste0("case_id_", dplyr::row_number()),
        dynamics_type = dynamics_type
      )

    community_seeds <- expt |>
      dplyr::select(community_id) |>
      dplyr::distinct() |>
      dplyr::mutate(community_seed = floor(stats::runif(dplyr::n()) * 1000000))

    temperature_seeds <- expt |>
      dplyr::select(env_series_id) |>
      dplyr::distinct() |>
      dplyr::mutate(temperature_seed = floor(stats::runif(dplyr::n()) * 1000000))

    expt <- expt |>
      dplyr::left_join(community_seeds, by = "community_id") |>
      dplyr::left_join(temperature_seeds, by = "env_series_id")

    community_object <- expt |>
      dplyr::mutate(case_id = dplyr::row_number()) |>
      dplyr::rowwise(case_id) |>
      dplyr::do(community_object = make_a_consumer_resource_community(
        S = .$richness,
        u_max_mean = .$u_max_mean,
        u_max_range = .$u_max_range,
        u_max_distribution = .$u_max_distribution,
        u_opt_mean = .$u_opt_mean,
        u_opt_range = .$u_opt_range,
        u_opt_distribution = .$u_opt_distribution,
        sd_u_mean = .$sd_u_mean,
        sd_u_range = .$sd_u_range,
        sd_u_distribution = .$sd_u_distribution,
        half_saturation_mean = .$half_saturation_mean,
        half_saturation_range = .$half_saturation_range,
        half_saturation_distribution = .$half_saturation_distribution,
        consumer_death_rate = .$consumer_death_rate,
        resource_renewal_rate = .$resource_renewal_rate,
        resource_supply = .$resource_supply,
        conversion_efficiency = .$conversion_efficiency,
        resource_use_mode = .$resource_use_mode,
        active_resource = .$active_resource,
        resource_specialization = .$resource_specialization,
        community_seed = .$community_seed
      ))

    expt <- cbind(expt, community_object)

    saveRDS(expt, output_path)
    announce_output_written(output_path, verbose = verbose, label = "experiment table")

    return(paste("Number of simulations in experiment is", nrow(expt)))
  }

  expt <- expand.grid(a_b_mean = eval(expt_def$a_b_mean_treatment),
                      a_b_range = eval(expt_def$a_b_range_treatment),
                      a_b_distribution = eval(expt_def$a_b_distribution),

                      b_opt_mean = eval(expt_def$b_opt_mean_treatment),
                      b_opt_range = eval(expt_def$b_opt_range_treatment),
                      b_opt_distribution = eval(expt_def$b_opt_distribution),

                      sd_perf_distribution = eval(expt_def$sd_perf_distribution),
                      sd_perf_mean=eval(expt_def$sd_perf_mean),
                      sd_perf_range=eval(expt_def$sd_perf_range),

                      alpha_ij_distribution = eval(expt_def$alpha_ij_distribution),
                      alpha_ij_mean = eval(expt_def$alpha_ij_mean_treatment),
                      alpha_ij_sd = eval(expt_def$alpha_ij_sd_treatment),

                      community_replicate = 1:eval(expt_def$number_of_community_replicates),

                      temperature_mean = eval(expt_def$temperature_mean),
                      temperature_sd = eval(expt_def$temperature_sd),
                      one_over_f_gamma = eval(expt_def$one_over_f_gamma),

                      temperature_replicate = 1:eval(expt_def$number_of_environment_replicates),

                      richness = eval(number_of_species)) |>
    dplyr::mutate(
      env_series_id = paste0("env_series_", temperature_mean, "_",
                             temperature_sd, "_",
                             one_over_f_gamma, "_",
                             temperature_replicate),
      community_id = paste0("community_", a_b_mean, "_",
                            a_b_range, "_",
                            a_b_distribution, "_",
                            b_opt_mean, "_",
                            b_opt_range, "_",
                            b_opt_distribution, "_",
                            sd_perf_distribution, "_",
                            sd_perf_mean, "_",
                            sd_perf_range, "_",
                            alpha_ij_distribution, "_",
                            alpha_ij_mean, "_",
                            alpha_ij_sd, "_",
                            community_replicate, "_",
                            richness),
      case_id = paste0("case_id_", dplyr::row_number())
    )

  community_seeds <- expt |>
    dplyr::select(community_id) |>
    dplyr::distinct() |>
    dplyr::mutate(community_seed = floor(stats::runif(dplyr::n()) * 1000000))

  temperature_seeds <- expt |>
    dplyr::select(env_series_id) |>
    dplyr::distinct() |>
    dplyr::mutate(temperature_seed = floor(stats::runif(dplyr::n()) * 1000000))

  expt <- expt |>
    dplyr::left_join(community_seeds, by = "community_id") |>
    dplyr::left_join(temperature_seeds, by = "env_series_id")


  if (!is.null(expt_def$sd_perf_curve)) {
    stop(
      "`sd_perf_curve` is deprecated. Use `sd_perf_mean` and `sd_perf_range` ",
      "to define standard-deviation performance-curve widths.",
      call. = FALSE
    )
  }

  #a_b <- eval(expt_def$a_b)
  a_d <- eval(expt_def$a_d)
  z <- eval(expt_def$z)
  alpha_jj <- eval(expt_def$alpha_jj)

  community_object <- expt |>
    dplyr::mutate(case_id = dplyr::row_number()) |>
    dplyr::rowwise(case_id) |>
    #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
    dplyr::do(community_object = make_a_community(S = .$richness,

                                           a_b_mean = .$a_b_mean,
                                           a_b_range = .$a_b_range,
                                           a_b_distribution = .$a_b_distribution,


                                           b_opt_mean = .$b_opt_mean,
                                           b_opt_range = .$b_opt_range,
                                           b_opt_distribution = .$b_opt_distribution,

                                           sd_perf_distribution = .$sd_perf_distribution,
                                           sd_perf_mean = .$sd_perf_mean,
                                           sd_perf_range = .$sd_perf_range,

                                           alpha_ij_mean = .$alpha_ij_mean,
                                           alpha_ij_sd = .$alpha_ij_sd,

                                           community_seed = .$community_seed,

                                           a_d = a_d,
                                           z = z,
                                           alpha_jj = alpha_jj,
                                           alpha_ij_distribution = .$alpha_ij_distribution
    ))

  expt <- cbind(expt, community_object)

  saveRDS(expt, output_path)
  announce_output_written(output_path, verbose = verbose, label = "experiment table")

  return(paste("Number of simulations in experiment is", nrow(expt)))

}
