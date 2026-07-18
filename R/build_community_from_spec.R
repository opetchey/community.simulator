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
    a_b_mean = traits$birth_maximum$mean,
    a_b_range = traits$birth_maximum$range,
    a_b_distribution = traits$birth_maximum$distribution,
    b_opt_mean = traits$birth_optimum$mean,
    b_opt_range = traits$birth_optimum$range,
    b_opt_distribution = traits$birth_optimum$distribution,
    sd_perf_distribution = traits$birth_width$distribution,
    sd_perf_mean = traits$birth_width$mean,
    sd_perf_range = traits$birth_width$range,
    community_seed = community_seed,
    a_d = traits$death$intercept,
    z = traits$death$temperature_slope,
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
    u_max_mean = traits$uptake_maximum$mean,
    u_max_range = traits$uptake_maximum$range,
    u_max_distribution = traits$uptake_maximum$distribution,
    u_opt_mean = traits$uptake_optimum$mean,
    u_opt_range = traits$uptake_optimum$range,
    u_opt_distribution = traits$uptake_optimum$distribution,
    sd_u_mean = traits$uptake_width$mean,
    sd_u_range = traits$uptake_width$range,
    sd_u_distribution = traits$uptake_width$distribution,
    half_saturation_mean = traits$half_saturation$mean,
    half_saturation_range = traits$half_saturation$range,
    half_saturation_distribution = traits$half_saturation$distribution,
    consumer_death_rate = resources$consumer_death_rate,
    resource_renewal_rate = resources$renewal_rate,
    resource_supply = resources$supply,
    conversion_efficiency = resources$conversion_efficiency,
    resource_use_mode = resources$use_mode,
    active_resource = resources$active_resource %||% 1,
    resource_specialization_distribution = private_use$distribution,
    resource_specialization_mean = private_use$mean,
    resource_specialization_range = private_use$range %||% 0,
    resource_specialization_precision = private_use$precision %||% 10,
    community_seed = community_seed
  )
}

#' Build an LV community object
#'
#' This is the rewrite-facing LV community constructor. It currently delegates
#' to the existing LV constructor while the remaining experiment workflow is
#' migrated to the canonical YAML specification.
#'
#' @inheritParams make_a_community
#'
#' @return An LV community parameter object.
#' @rdname build_community_constructors
#' @export
build_LV_community <- function(S,
                               a_b_mean,
                               a_b_range,
                               a_b_distribution,
                               b_opt_mean,
                               b_opt_range,
                               b_opt_distribution,
                               sd_perf_distribution,
                               sd_perf_mean,
                               sd_perf_range,
                               alpha_ij_mean = NULL,
                               alpha_ij_sd = NULL,
                               community_seed,
                               a_d,
                               z,
                               alpha_jj = 1,
                               alpha_ij_distribution = NULL,
                               lv_interaction_spec = NULL) {
  make_a_community(
    S = S,
    a_b_mean = a_b_mean,
    a_b_range = a_b_range,
    a_b_distribution = a_b_distribution,
    b_opt_mean = b_opt_mean,
    b_opt_range = b_opt_range,
    b_opt_distribution = b_opt_distribution,
    sd_perf_distribution = sd_perf_distribution,
    sd_perf_mean = sd_perf_mean,
    sd_perf_range = sd_perf_range,
    alpha_ij_mean = alpha_ij_mean,
    alpha_ij_sd = alpha_ij_sd,
    community_seed = community_seed,
    a_d = a_d,
    z = z,
    alpha_jj = alpha_jj,
    alpha_ij_distribution = alpha_ij_distribution,
    lv_interaction_spec = lv_interaction_spec
  )
}

#' Build a CR community object
#'
#' This is the rewrite-facing consumer-resource community constructor. It
#' currently delegates to the existing CR constructor while the remaining
#' experiment workflow is migrated to the canonical YAML specification.
#'
#' @inheritParams make_a_consumer_resource_community
#'
#' @return A CR community parameter object.
#' @rdname build_community_constructors
#' @export
build_CR_community <- function(S,
                               u_max_mean,
                               u_max_range,
                               u_max_distribution,
                               u_opt_mean,
                               u_opt_range,
                               u_opt_distribution,
                               sd_u_mean,
                               sd_u_range,
                               sd_u_distribution,
                               half_saturation_mean,
                               half_saturation_range,
                               half_saturation_distribution,
                               consumer_death_rate,
                               resource_renewal_rate,
                               resource_supply,
                               conversion_efficiency,
                               resource_use_mode = "one_resource_all_consumers",
                               active_resource = 1,
                               resource_specialization = 1,
                               resource_specialization_distribution = "constant",
                               resource_specialization_mean = resource_specialization,
                               resource_specialization_range = 0,
                               resource_specialization_precision = 10,
                               community_seed) {
  make_a_consumer_resource_community(
    S = S,
    u_max_mean = u_max_mean,
    u_max_range = u_max_range,
    u_max_distribution = u_max_distribution,
    u_opt_mean = u_opt_mean,
    u_opt_range = u_opt_range,
    u_opt_distribution = u_opt_distribution,
    sd_u_mean = sd_u_mean,
    sd_u_range = sd_u_range,
    sd_u_distribution = sd_u_distribution,
    half_saturation_mean = half_saturation_mean,
    half_saturation_range = half_saturation_range,
    half_saturation_distribution = half_saturation_distribution,
    consumer_death_rate = consumer_death_rate,
    resource_renewal_rate = resource_renewal_rate,
    resource_supply = resource_supply,
    conversion_efficiency = conversion_efficiency,
    resource_use_mode = resource_use_mode,
    active_resource = active_resource,
    resource_specialization = resource_specialization,
    resource_specialization_distribution = resource_specialization_distribution,
    resource_specialization_mean = resource_specialization_mean,
    resource_specialization_range = resource_specialization_range,
    resource_specialization_precision = resource_specialization_precision,
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
