#' Build a community object from a resolved case specification
#'
#' Builds the model-specific community parameter object for one resolved
#' `case_spec` from the canonical YAML experiment-table workflow.
#'
#' @param case_spec A resolved case specification, usually from the `case_spec`
#'   list-column produced by [create_experiment_table_from_spec()].
#' @param community_seed Optional random seed for community construction. When
#'   omitted, `case_spec$community$seed` is used, falling back to
#'   `case_spec$experiment$random_seed`.
#'
#' @return A model-specific community parameter object.
#' @export
#'
#' @examples
#' template <- system.file(
#'   "experiment_templates/lv_discrete.yaml",
#'   package = "community.simulator"
#' )
#' if (nzchar(template)) {
#'   table <- create_experiment_table_from_spec(template)
#'   community <- build_community_from_spec(table$case_spec[[1]])
#' }
build_community_from_spec <- function(case_spec, community_seed = NULL) {
  validate_experiment_spec(case_spec)
  model_type <- case_spec$model$type

  if (startsWith(model_type, "lv_")) {
    return(build_LV_community_from_spec(case_spec, community_seed = community_seed))
  }
  if (identical(model_type, "consumer_resource_continuous")) {
    return(build_CR_community_from_spec(
      case_spec,
      community_seed = community_seed
    ))
  }

  stop("Unsupported model type: ", model_type, call. = FALSE)
}

#' @rdname build_community_from_spec
#' @export
build_LV_community_from_spec <- function(case_spec, community_seed = NULL) {
  community_seed <- resolve_community_seed(case_spec, community_seed)
  traits <- case_spec$traits

  build_LV_community(
    S = as.integer(case_spec$community$richness),
    birth_maximum_mean = traits$birth_maximum$mean,
    birth_maximum_range = traits$birth_maximum$range,
    birth_maximum_distribution = traits$birth_maximum$distribution,
    birth_optimum_mean = traits$birth_optimum$mean,
    birth_optimum_range = traits$birth_optimum$range,
    birth_optimum_distribution = traits$birth_optimum$distribution,
    birth_width_distribution = traits$birth_width$distribution,
    birth_width_mean = traits$birth_width$mean,
    birth_width_range = traits$birth_width$range,
    community_seed = community_seed,
    death_intercept = traits$death$intercept,
    death_temperature_slope = traits$death$temperature_slope,
    lv_interaction_spec = case_spec$interactions$selected
  )
}

#' @rdname build_community_from_spec
#' @export
build_CR_community_from_spec <- function(case_spec, community_seed = NULL) {
  community_seed <- resolve_community_seed(case_spec, community_seed)
  traits <- case_spec$traits
  resources <- case_spec$resources
  private_use <- resources$private_use %||% list(
    distribution = "constant",
    mean = 1,
    range = 0,
    precision = 10
  )

  build_CR_community(
    S = as.integer(case_spec$community$richness),
    uptake_maximum_mean = traits$uptake_maximum$mean,
    uptake_maximum_range = traits$uptake_maximum$range,
    uptake_maximum_distribution = traits$uptake_maximum$distribution,
    uptake_optimum_mean = traits$uptake_optimum$mean,
    uptake_optimum_range = traits$uptake_optimum$range,
    uptake_optimum_distribution = traits$uptake_optimum$distribution,
    uptake_width_mean = traits$uptake_width$mean,
    uptake_width_range = traits$uptake_width$range,
    uptake_width_distribution = traits$uptake_width$distribution,
    half_saturation_mean = traits$half_saturation$mean,
    half_saturation_range = traits$half_saturation$range,
    half_saturation_distribution = traits$half_saturation$distribution,
    consumer_death_rate = resources$consumer_death_rate,
    resource_renewal_rate = resources$renewal_rate,
    resource_supply = resources$supply,
    conversion_efficiency = resources$conversion_efficiency,
    resource_use_mode = resources$use_mode,
    active_resource = resources$active_resource %||% 1,
    private_resource_use_distribution = private_use$distribution,
    private_resource_use_mean = private_use$mean,
    private_resource_use_range = private_use$range %||% 0,
    private_resource_use_precision = private_use$precision %||% 10,
    community_seed = community_seed
  )
}

#' Build an LV community object
#'
#' This is the rewrite-facing LV community constructor. It uses the same
#' parameter names as the YAML experiment schema and delegates to the existing
#' LV constructor at the adapter boundary.
#'
#' @param S Number of species in the community.
#' @param birth_maximum_mean Mean maximum birth rate.
#' @param birth_maximum_range Range of maximum birth rates.
#' @param birth_maximum_distribution Distribution for maximum birth rates.
#' @param birth_optimum_mean Mean birth-rate thermal optimum.
#' @param birth_optimum_range Range of birth-rate thermal optima.
#' @param birth_optimum_distribution Distribution for birth-rate thermal optima.
#' @param birth_width_distribution Distribution for Gaussian birth-curve widths.
#' @param birth_width_mean Mean Gaussian birth-curve width.
#' @param birth_width_range Range of Gaussian birth-curve widths.
#' @param death_intercept Death rate when temperature is equal to 0.
#' @param death_temperature_slope Exponential temperature sensitivity of death.
#' @param community_seed Random seed used when generating community traits.
#' @param lv_interaction_spec Optional named list specifying LV interactions.
#'
#' @return An LV community parameter object.
#' @rdname build_community_constructors
#' @export
build_LV_community <- function(S,
                               birth_maximum_mean,
                               birth_maximum_range,
                               birth_maximum_distribution,
                               birth_optimum_mean,
                               birth_optimum_range,
                               birth_optimum_distribution,
                               birth_width_distribution,
                               birth_width_mean,
                               birth_width_range,
                               community_seed,
                               death_intercept,
                               death_temperature_slope,
                               lv_interaction_spec = NULL) {
  make_a_community(
    S = S,
    a_b_mean = birth_maximum_mean,
    a_b_range = birth_maximum_range,
    a_b_distribution = birth_maximum_distribution,
    b_opt_mean = birth_optimum_mean,
    b_opt_range = birth_optimum_range,
    b_opt_distribution = birth_optimum_distribution,
    sd_perf_distribution = birth_width_distribution,
    sd_perf_mean = birth_width_mean,
    sd_perf_range = birth_width_range,
    alpha_ij_mean = NULL,
    alpha_ij_sd = NULL,
    community_seed = community_seed,
    a_d = death_intercept,
    z = death_temperature_slope,
    alpha_jj = 1,
    alpha_ij_distribution = NULL,
    lv_interaction_spec = lv_interaction_spec
  )
}

#' Build a CR community object
#'
#' This is the rewrite-facing consumer-resource community constructor. It uses
#' the same parameter names as the YAML experiment schema and delegates to the
#' existing CR constructor at the adapter boundary.
#'
#' @param S Number of consumer species.
#' @param uptake_maximum_mean Mean maximum uptake height.
#' @param uptake_maximum_range Range of maximum uptake heights.
#' @param uptake_maximum_distribution Distribution for maximum uptake heights.
#' @param uptake_optimum_mean Mean uptake thermal optimum.
#' @param uptake_optimum_range Range of uptake thermal optima.
#' @param uptake_optimum_distribution Distribution for uptake thermal optima.
#' @param uptake_width_mean Mean Gaussian uptake-curve width.
#' @param uptake_width_range Range of Gaussian uptake-curve widths.
#' @param uptake_width_distribution Distribution for Gaussian uptake-curve widths.
#' @param half_saturation_mean Mean Monod half-saturation constant.
#' @param half_saturation_range Range of half-saturation constants.
#' @param half_saturation_distribution Distribution for half-saturation constants.
#' @param consumer_death_rate Consumer death rate.
#' @param resource_renewal_rate Chemostat resource renewal rate.
#' @param resource_supply Resource supply concentration.
#' @param conversion_efficiency Conversion efficiency from uptake to consumer
#'   growth.
#' @param resource_use_mode Resource-use mode.
#' @param active_resource Active shared resource index.
#' @param private_resource_use Mean private-resource use when a scalar default
#'   is needed.
#' @param private_resource_use_distribution Distribution used to generate
#'   species-level private-resource use.
#' @param private_resource_use_mean Mean species-level private-resource use.
#' @param private_resource_use_range Range for regular or uniform
#'   private-resource use.
#' @param private_resource_use_precision Precision for beta-distributed
#'   private-resource use.
#' @param community_seed Random seed used when generating community traits.
#'
#' @return A CR community parameter object.
#' @rdname build_community_constructors
#' @export
build_CR_community <- function(S,
                               uptake_maximum_mean,
                               uptake_maximum_range,
                               uptake_maximum_distribution,
                               uptake_optimum_mean,
                               uptake_optimum_range,
                               uptake_optimum_distribution,
                               uptake_width_mean,
                               uptake_width_range,
                               uptake_width_distribution,
                               half_saturation_mean,
                               half_saturation_range,
                               half_saturation_distribution,
                               consumer_death_rate,
                               resource_renewal_rate,
                               resource_supply,
                               conversion_efficiency,
                               resource_use_mode = "one_resource_all_consumers",
                               active_resource = 1,
                               private_resource_use = 1,
                               private_resource_use_distribution = "constant",
                               private_resource_use_mean = private_resource_use,
                               private_resource_use_range = 0,
                               private_resource_use_precision = 10,
                               community_seed) {
  make_a_consumer_resource_community(
    S = S,
    u_max_mean = uptake_maximum_mean,
    u_max_range = uptake_maximum_range,
    u_max_distribution = uptake_maximum_distribution,
    u_opt_mean = uptake_optimum_mean,
    u_opt_range = uptake_optimum_range,
    u_opt_distribution = uptake_optimum_distribution,
    sd_u_mean = uptake_width_mean,
    sd_u_range = uptake_width_range,
    sd_u_distribution = uptake_width_distribution,
    half_saturation_mean = half_saturation_mean,
    half_saturation_range = half_saturation_range,
    half_saturation_distribution = half_saturation_distribution,
    consumer_death_rate = consumer_death_rate,
    resource_renewal_rate = resource_renewal_rate,
    resource_supply = resource_supply,
    conversion_efficiency = conversion_efficiency,
    resource_use_mode = resource_use_mode,
    active_resource = active_resource,
    resource_specialization = private_resource_use,
    resource_specialization_distribution = private_resource_use_distribution,
    resource_specialization_mean = private_resource_use_mean,
    resource_specialization_range = private_resource_use_range,
    resource_specialization_precision = private_resource_use_precision,
    community_seed = community_seed
  )
}

resolve_community_seed <- function(case_spec, community_seed = NULL) {
  if (!is.null(community_seed)) {
    return(as.integer(community_seed))
  }
  if (!is.null(case_spec$community$seed)) {
    return(as.integer(case_spec$community$seed))
  }
  as.integer(case_spec$experiment$random_seed)
}
