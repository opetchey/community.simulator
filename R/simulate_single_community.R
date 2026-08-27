#' Build a single exploratory community
#'
#' @param model_type Population dynamic model. One of `"lv_discrete"`,
#'   `"lv_continuous"`, or `"consumer_resource_continuous"`.
#' @param richness Number of species or consumers.
#' @param random_seed Random seed for community generation.
#' @param lv_interaction LV interaction preset. Used for LV models.
#' @param lv_interaction_type Optional detailed LV interaction type. If
#'   supplied, this overrides `lv_interaction`.
#' @param lv_interaction_symmetry Detailed LV interaction symmetry.
#' @param lv_interaction_distribution Detailed LV interaction distribution.
#' @param lv_interaction_min Minimum off-diagonal interaction value for detailed
#'   uniform LV interactions.
#' @param lv_interaction_max Maximum off-diagonal interaction value for detailed
#'   uniform LV interactions.
#' @param lv_interaction_value Constant off-diagonal interaction value for
#'   detailed constant LV interactions.
#' @param lv_interaction_diagonal Diagonal value of the detailed LV interaction
#'   matrix.
#' @param private_resource_use_mean Mean private-resource use fraction for the
#'   consumer-resource model when using the shared-to-private resource mode.
#' @param private_resource_use_distribution Distribution used to generate
#'   private-resource use fractions.
#' @param private_resource_use_range Range of private-resource use fractions for
#'   regular and random-uniform distributions.
#' @param private_resource_use_precision Precision for beta-distributed
#'   private-resource use fractions.
#' @param resource_use_mode Consumer-resource resource-use mode.
#' @param active_resource Active or shared resource index.
#' @param half_saturation_mean Mean Monod half-saturation constant.
#' @param consumer_death_rate Consumer death rate.
#' @param resource_renewal_rate Resource renewal rate.
#' @param resource_supply Resource supply concentration.
#' @param conversion_efficiency Conversion efficiency from uptake to growth.
#' @param birth_maximum_mean Mean maximum birth rate for LV models.
#' @param birth_optimum_mean Mean birth thermal optimum for LV models.
#' @param birth_optimum_range Range of birth thermal optima for LV models.
#' @param birth_width_mean Mean birth performance-curve width for LV models.
#' @param uptake_maximum_mean Mean maximum uptake rate for the CR model.
#' @param uptake_optimum_mean Mean uptake thermal optimum for the CR model.
#' @param uptake_optimum_range Range of uptake thermal optima for the CR model.
#' @param uptake_width_mean Mean uptake performance-curve width for the CR model.
#' @param uptake_width_range Range of uptake performance-curve widths for the CR
#'   model.
#'
#' @return A list with the community object, model type, trait table,
#'   community-structure matrix data, species performance curves, and a summed
#'   community performance curve.
#' @export
#'
#' @examples
#' community <- build_single_community(model_type = "lv_discrete", richness = 3)
#' names(community)
#' community$traits
build_single_community <- function(model_type = c(
                                     "lv_discrete",
                                     "lv_continuous",
                                     "consumer_resource_continuous"
                                   ),
                                   richness = 4,
                                   random_seed = 1,
                                   lv_interaction = c(
                                     "none",
                                     "weak_asymmetric_competition",
                                     "weak_symmetric_competition",
                                     "predator_prey"
                                   ),
                                   lv_interaction_type = NULL,
                                   lv_interaction_symmetry = "asymmetric",
                                   lv_interaction_distribution = "uniform",
                                   lv_interaction_min = 0,
                                   lv_interaction_max = 0.2,
                                   lv_interaction_value = 0,
                                   lv_interaction_diagonal = 1,
                                   private_resource_use_mean = 0,
                                   private_resource_use_distribution = "constant",
                                   private_resource_use_range = 0,
                                   private_resource_use_precision = 12,
                                   resource_use_mode = "shared_to_private",
                                   active_resource = 1,
                                   half_saturation_mean = 100,
                                   consumer_death_rate = 0.181532,
                                   resource_renewal_rate = 6.051066,
                                   resource_supply = 1000,
                                   conversion_efficiency = 1,
                                   birth_maximum_mean = 0.3,
                                   birth_optimum_mean = 20,
                                   birth_optimum_range = 6,
                                   birth_width_mean = 8,
                                   uptake_maximum_mean = 0.363064,
                                   uptake_optimum_mean = 16,
                                   uptake_optimum_range = 0,
                                   uptake_width_mean = 1,
                                   uptake_width_range = 0.5) {
  model_type <- match.arg(model_type)
  lv_interaction <- match.arg(lv_interaction)
  richness <- as.integer(richness)
  if (is.na(richness) || richness < 1) {
    stop("`richness` must be a positive integer.", call. = FALSE)
  }

  if (model_type %in% c("lv_discrete", "lv_continuous")) {
    if (!is.null(lv_interaction_type) && nzchar(lv_interaction_type)) {
      interaction_parameters <- if (lv_interaction_distribution == "constant") {
        list(value = lv_interaction_value)
      } else {
        list(min = lv_interaction_min, max = lv_interaction_max)
      }
      interaction_spec <- list(
        label = "detailed_interaction",
        type = lv_interaction_type,
        symmetry = lv_interaction_symmetry,
        distribution = lv_interaction_distribution,
        parameters = interaction_parameters,
        diagonal = lv_interaction_diagonal
      )
    } else {
      interaction_spec <- switch(
        lv_interaction,
        none = list(
          label = "no_interactions",
          type = "none",
          diagonal = 1
        ),
        weak_asymmetric_competition = list(
          label = "weak_asymmetric_competition",
          type = "competition",
          symmetry = "asymmetric",
          distribution = "uniform",
          parameters = list(min = 0, max = 0.2),
          diagonal = 1
        ),
        weak_symmetric_competition = list(
          label = "weak_symmetric_competition",
          type = "competition",
          symmetry = "symmetric",
          distribution = "uniform",
          parameters = list(min = 0, max = 0.2),
          diagonal = 1
        ),
        predator_prey = list(
          label = "predator_prey",
          type = "predator_prey",
          symmetry = "antisymmetric",
          distribution = "uniform",
          parameters = list(min = 0, max = 0.2),
          diagonal = 1
        )
      )
    }

    community <- make_a_community(
      S = richness,
      a_b_mean = birth_maximum_mean,
      a_b_range = 0,
      a_b_distribution = "regular",
      b_opt_mean = birth_optimum_mean,
      b_opt_range = birth_optimum_range,
      b_opt_distribution = "regular",
      sd_perf_distribution = "regular",
      sd_perf_mean = birth_width_mean,
      sd_perf_range = 0,
      community_seed = random_seed,
      a_d = 0,
      z = 0.05,
      lv_interaction_spec = interaction_spec
    )

    trait_table <- tibble::tibble(
      species = paste0("Spp", seq_len(community$S)),
      birth_maximum = community$a_b_i,
      birth_optimum = community$b_opt_i,
      birth_width = community$sd_perf_i,
      death_intercept = community$a_d_i,
      death_temperature_slope = community$z_i
    )
    structure_matrix <- matrix_to_plot_data(
      community$alpha_ij,
      row_prefix = "Species ",
      column_prefix = "Species ",
      value_name = "value"
    )
    structure_matrix$matrix_type <- "LV interaction"
  } else {
    community <- make_a_consumer_resource_community(
      S = richness,
      u_max_mean = uptake_maximum_mean,
      u_max_range = 0,
      u_max_distribution = "random_uniform",
      u_opt_mean = uptake_optimum_mean,
      u_opt_range = uptake_optimum_range,
      u_opt_distribution = "random_uniform",
      sd_u_mean = uptake_width_mean,
      sd_u_range = uptake_width_range,
      sd_u_distribution = "random_uniform",
      half_saturation_mean = half_saturation_mean,
      half_saturation_range = 0,
      half_saturation_distribution = "random_uniform",
      consumer_death_rate = consumer_death_rate,
      resource_renewal_rate = resource_renewal_rate,
      resource_supply = resource_supply,
      conversion_efficiency = conversion_efficiency,
      resource_use_mode = resource_use_mode,
      active_resource = active_resource,
      resource_specialization_distribution = private_resource_use_distribution,
      resource_specialization_mean = private_resource_use_mean,
      resource_specialization_range = private_resource_use_range,
      resource_specialization_precision = private_resource_use_precision,
      community_seed = random_seed
    )

    trait_table <- tibble::tibble(
      species = paste0("Spp", seq_len(community$S)),
      uptake_maximum = community$u_max_i,
      uptake_optimum = community$u_opt_i,
      uptake_width = community$sd_u_i,
      private_resource_use = community$resource_specialization_i,
      shared_resource_use = 1 - community$resource_specialization_i
    )
    structure_matrix <- matrix_to_plot_data(
      community$resource_use_ij,
      row_prefix = "Consumer ",
      column_prefix = "Resource ",
      value_name = "value"
    )
    structure_matrix$matrix_type <- "Resource use"
  }

  temperature_min <- min(community$b_opt_i - 3 * community$sd_perf_i)
  temperature_max <- max(community$b_opt_i + 3 * community$sd_perf_i)
  temperatures <- seq(temperature_min, temperature_max, length.out = 200)
  performance_curves <- expand.grid(
    species_index = seq_len(community$S),
    temperature = temperatures
  )
  performance_curves$species <- paste0("Spp", performance_curves$species_index)
  performance_curves$performance <- with(
    performance_curves,
    community$a_b_i[species_index] *
      exp(-0.5 * ((temperature - community$b_opt_i[species_index]) /
        community$sd_perf_i[species_index])^2) -
      community$a_d_i[species_index] *
        exp(community$z_i[species_index] * temperature)
  )
  performance_curves$viable <- performance_curves$performance > 0
  performance_curves$model_type <- model_type

  if (model_type == "consumer_resource_continuous") {
    community_performance_curve <- stats::aggregate(
      viable ~ temperature,
      data = performance_curves,
      FUN = sum
    )
    names(community_performance_curve) <- c(
      "temperature",
      "community_performance"
    )
    community_performance_curve$community_performance <-
      community_performance_curve$community_performance / community$S
    community_performance_curve$performance_type <- "viability_fraction"
    community_performance_curve$y_label <- "Fraction viable"
  } else {
    community_performance_curve <- stats::aggregate(
      performance ~ temperature,
      data = performance_curves,
      FUN = sum
    )
    names(community_performance_curve) <- c(
      "temperature",
      "community_performance"
    )
    community_performance_curve$performance_type <- "summed_performance"
    community_performance_curve$y_label <- "Summed community performance"
  }

  list(
    model_type = model_type,
    community = community,
    traits = trait_table,
    structure_matrix = structure_matrix,
    performance_curves = tibble::as_tibble(performance_curves),
    community_performance_curve = tibble::as_tibble(community_performance_curve)
  )
}

create_single_temperature_series <- function(experiment_duration,
                                             temperature_mean,
                                             temperature_sd,
                                             one_over_f_gamma,
                                             random_seed) {
  experiment_duration <- as.integer(experiment_duration)
  if (is.na(experiment_duration) || experiment_duration < 2) {
    stop("`experiment_duration` must be at least 2.", call. = FALSE)
  }

  set.seed(random_seed)
  tibble::tibble(
    time = seq_len(experiment_duration),
    temperature = generate_one_over_f_temperature(
      n = experiment_duration,
      mean = temperature_mean,
      sd = temperature_sd,
      gamma = one_over_f_gamma
    )
  )
}

#' Simulate one exploratory community
#'
#' This helper is intended for examples and the Shiny simulation explorer. It
#' defines one community, generates one temperature time series, simulates one
#' case, and returns tidy data frames for plotting.
#'
#' @inheritParams build_single_community
#' @param experiment_duration Number of time points to simulate.
#' @param temperature_mean Mean temperature.
#' @param temperature_sd Standard deviation of the temperature series.
#' @param one_over_f_gamma Slope parameter for the `1/f` temperature process.
#' @param initial_total_abundance Initial total abundance distributed evenly
#'   across species or consumers.
#' @param resource_initial_value Initial value for each resource in the CR
#'   model.
#' @param immigration_rate Immigration rate for continuous LV dynamics.
#' @param consumer_immigration_rate Immigration rate for CR consumers.
#'
#' @return A list with community, temperature, abundance, resource, total
#'   abundance, and summary outputs.
#' @export
#'
#' @examples
#' result <- simulate_single_community(
#'   model_type = "lv_discrete",
#'   richness = 3,
#'   experiment_duration = 10
#' )
#' names(result)
simulate_single_community <- function(model_type = c(
                                        "lv_discrete",
                                        "lv_continuous",
                                        "consumer_resource_continuous"
                                      ),
                                      richness = 4,
                                      random_seed = 1,
                                      lv_interaction = c(
                                        "none",
                                        "weak_asymmetric_competition",
                                        "weak_symmetric_competition",
                                        "predator_prey"
                                      ),
                                      lv_interaction_type = NULL,
                                      lv_interaction_symmetry = "asymmetric",
                                      lv_interaction_distribution = "uniform",
                                      lv_interaction_min = 0,
                                      lv_interaction_max = 0.2,
                                      lv_interaction_value = 0,
                                      lv_interaction_diagonal = 1,
                                      private_resource_use_mean = 0,
                                      private_resource_use_distribution = "constant",
                                      private_resource_use_range = 0,
                                      private_resource_use_precision = 12,
                                      resource_use_mode = "shared_to_private",
                                      active_resource = 1,
                                      half_saturation_mean = 100,
                                      consumer_death_rate = 0.181532,
                                      resource_renewal_rate = 6.051066,
                                      resource_supply = 1000,
                                      conversion_efficiency = 1,
                                      birth_maximum_mean = 0.3,
                                      birth_optimum_mean = 20,
                                      birth_optimum_range = 6,
                                      birth_width_mean = 8,
                                      uptake_maximum_mean = 0.363064,
                                      uptake_optimum_mean = 16,
                                      uptake_optimum_range = 0,
                                      uptake_width_mean = 1,
                                      uptake_width_range = 0.5,
                                      experiment_duration = 60,
                                      temperature_mean = 20,
                                      temperature_sd = 1,
                                      one_over_f_gamma = 0.8,
                                      initial_total_abundance = 100,
                                      resource_initial_value = 1000,
                                      immigration_rate = 0.1,
                                      consumer_immigration_rate = 0.01) {
  model_type <- match.arg(model_type)
  lv_interaction <- match.arg(lv_interaction)

  built <- build_single_community(
    model_type = model_type,
    richness = richness,
    random_seed = random_seed,
    lv_interaction = lv_interaction,
    lv_interaction_type = lv_interaction_type,
    lv_interaction_symmetry = lv_interaction_symmetry,
    lv_interaction_distribution = lv_interaction_distribution,
    lv_interaction_min = lv_interaction_min,
    lv_interaction_max = lv_interaction_max,
    lv_interaction_value = lv_interaction_value,
    lv_interaction_diagonal = lv_interaction_diagonal,
    private_resource_use_mean = private_resource_use_mean,
    private_resource_use_distribution = private_resource_use_distribution,
    private_resource_use_range = private_resource_use_range,
    private_resource_use_precision = private_resource_use_precision,
    resource_use_mode = resource_use_mode,
    active_resource = active_resource,
    half_saturation_mean = half_saturation_mean,
    consumer_death_rate = consumer_death_rate,
    resource_renewal_rate = resource_renewal_rate,
    resource_supply = resource_supply,
    conversion_efficiency = conversion_efficiency,
    birth_maximum_mean = birth_maximum_mean,
    birth_optimum_mean = birth_optimum_mean,
    birth_optimum_range = birth_optimum_range,
    birth_width_mean = birth_width_mean,
    uptake_maximum_mean = uptake_maximum_mean,
    uptake_optimum_mean = uptake_optimum_mean,
    uptake_optimum_range = uptake_optimum_range,
    uptake_width_mean = uptake_width_mean,
    uptake_width_range = uptake_width_range
  )
  community <- built$community
  temperature <- create_single_temperature_series(
    experiment_duration = experiment_duration,
    temperature_mean = temperature_mean,
    temperature_sd = temperature_sd,
    one_over_f_gamma = one_over_f_gamma,
    random_seed = random_seed + 1
  )
  temperature_matrix <- matrix(temperature$temperature, nrow = 1)
  output_times <- temperature$time
  initial_abundances <- rep(initial_total_abundance / community$S, community$S)

  resources <- NULL
  if (model_type == "lv_discrete") {
    simulated <- simulator_lv_discrete(
      input_com_params = community,
      TcelSeries = temperature_matrix,
      initial_abundances = initial_abundances,
      immigration_rate = immigration_rate
    )
    abundances <- simulated |>
      tibble::as_tibble() |>
      dplyr::mutate(time = output_times) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("Spp"),
        names_to = "species",
        values_to = "abundance"
      )
  } else if (model_type == "lv_continuous") {
    simulated <- simulator_lv_continuous(
      input_com_params = community,
      TcelSeries = temperature_matrix,
      initial_abundances = initial_abundances,
      times = output_times,
      output_times = output_times,
      immigration_rate = immigration_rate
    )
    abundances <- simulated |>
      tibble::as_tibble() |>
      dplyr::mutate(time = output_times) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("Spp"),
        names_to = "species",
        values_to = "abundance"
      )
  } else {
    simulated <- simulator_consumer_resource_continuous(
      input_com_params = community,
      TcelSeries = temperature_matrix,
      initial_consumer_abundances = initial_abundances,
      initial_resource_values = rep(resource_initial_value, community$R),
      times = output_times,
      output_times = output_times,
      consumer_immigration_rate = consumer_immigration_rate
    )
    abundances <- simulated$consumers |>
      tibble::as_tibble() |>
      dplyr::mutate(time = output_times) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("Spp"),
        names_to = "species",
        values_to = "abundance"
      )
    resources <- simulated$resources |>
      tibble::as_tibble() |>
      dplyr::mutate(time = output_times) |>
      tidyr::pivot_longer(
        cols = dplyr::starts_with("Res"),
        names_to = "resource",
        values_to = "amount"
      )
  }

  total_abundance <- stats::aggregate(
    abundance ~ time,
    data = abundances,
    FUN = sum,
    na.rm = TRUE
  )
  names(total_abundance) <- c("time", "total_abundance")
  summary <- tibble::tibble(
    model_type = model_type,
    richness = community$S,
    duration = experiment_duration,
    mean_total_abundance = mean(total_abundance$total_abundance, na.rm = TRUE),
    cv_total_abundance = stats::sd(total_abundance$total_abundance, na.rm = TRUE) /
      mean(total_abundance$total_abundance, na.rm = TRUE),
    final_total_abundance = total_abundance$total_abundance[nrow(total_abundance)]
  )

  c(
    built,
    list(
      temperature = temperature,
      abundances = abundances,
      total_abundance = total_abundance,
      resources = resources,
      summary = summary
    )
  )
}
