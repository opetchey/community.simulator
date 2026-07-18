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

  mean_total_abundance <- mean(total_abundance, na.rm = TRUE)
  sd_total_abundance <- stats::sd(total_abundance, na.rm = TRUE)
  sum_population_sds <- sum(population_sds, na.rm = TRUE)
  relative_abundances <- population_means / mean_total_abundance

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
    mean_total_abundance = mean_total_abundance,
    sd_total_abundance = sd_total_abundance,
    cv_total_abundance = sd_total_abundance / mean_total_abundance,
    comm_temperature_sensitivity = temp_sensitivity,
    synchrony_abundance = sd_total_abundance^2 / sum_population_sds^2,
    mean_population_cv_abundance = sum(relative_abundances * population_cvs, na.rm = TRUE),
    final_total_abundance = total_abundance[[length(total_abundance)]],
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

stable_integer_from_character <- function(x) {
  x <- as.character(x)
  code_points <- utf8ToInt(x)
  if (length(code_points) == 0 || anyNA(code_points)) {
    return(0L)
  }
  hash <- 0
  modulus <- 1000000007
  for (code_point in code_points) {
    hash <- (hash * 131 + code_point) %% modulus
  }
  as.integer(hash)
}

initial_abundance_seed_for_case <- function(initial_abundance_seed_base,
                                            expt,
                                            i) {
  if (is.null(initial_abundance_seed_base)) {
    return(NULL)
  }
  seed_id <- if ("community_id" %in% names(expt) && !is.na(expt$community_id[[i]])) {
    expt$community_id[[i]]
  } else {
    paste0("case_", i)
  }
  as.integer(initial_abundance_seed_base) + stable_integer_from_character(seed_id)
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
    set.seed(initial_abundance_seed_for_case(initial_abundance_seed_base, expt, i))
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
