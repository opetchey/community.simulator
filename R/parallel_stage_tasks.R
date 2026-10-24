create_prepared_environment <- function(environments, settings, i) {
  set.seed(environments$temperature_seed[i])
  temperature_mean <- environments$temperature_mean[i]
  temperature_sd <- environments$temperature_sd[i]
  one_over_f_gamma <- environments$one_over_f_gamma[i]

  sample_times <- environment_sample_times(settings)
  tibble::tibble(
    phase = "expt",
    time = sample_times,
    temperature = generate_one_over_f_temperature(
      n = length(sample_times),
      mean = temperature_mean,
      sd = temperature_sd,
      gamma = one_over_f_gamma
    ),
    env_series_id = environments$env_series_id[i]
  )
}


simulate_prepared_case <- function(expt, community_objects, temperatures_data, settings, i) {
  expt_def <- list(burn_in_duration = settings$burn_in_duration,
                   experiment_duration = settings$experiment_duration,
                   temperature_mean = settings$temperature_mean)
  save_resources <- settings$save_resources && settings$dynamics_type == "consumer_resource_continuous"
  rate_scale <- if ("rate_scale" %in% names(expt)) expt$rate_scale[[i]] else settings$rate_scale
  simulate_one_dynamics_case(
    i = i,
    expt = expt,
    community_objects = community_objects,
    temperatures_data = temperatures_data,
    expt_def = expt_def,
    dynamics_type = settings$dynamics_type,
    temperature_interpolation = settings$temperature_interpolation,
    immigration_rate = settings$immigration_rate * rate_scale,
    consumer_immigration_rate = settings$consumer_immigration_rate * rate_scale,
    initial_consumer_total_abundance = settings$initial_consumer_total_abundance,
    resource_initial_value = settings$resource_initial_value,
    immigration_mode = settings$immigration_mode,
    ode_rhs = settings$ode_rhs,
    ode_method = settings$ode_method,
    ode_rtol = settings$ode_rtol,
    ode_atol = settings$ode_atol,
    ode_max_step = settings$ode_max_step,
    ode_maxsteps = settings$ode_maxsteps,
    blowup_threshold = settings$blowup_threshold,
    negative_tolerance = settings$negative_tolerance,
    dynamics_save_every = settings$dynamics_save_every,
    resources_save_every = settings$resources_save_every,
    save_dynamics = settings$save_dynamics,
    save_resources = save_resources,
    record_dynamics_wall_time = settings$record_dynamics_wall_time,
    extinction_threshold = settings$extinction_threshold,
    initial_abundance_seed_base = settings$initial_abundance_seed_base
  )
}

