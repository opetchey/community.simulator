#' Make a consumer-resource community parameter object
#'
#' @param S Number of consumer species. The number of resources is set equal to
#'   `S`, except when `resource_use_mode = "shared_to_private"`, where the
#'   number of resources is `S + 1`.
#' @param u_max_mean Mean maximum uptake height.
#' @param u_max_range Range of maximum uptake heights.
#' @param u_max_distribution Distribution for maximum uptake heights.
#' @param u_opt_mean Mean thermal optimum for maximum uptake.
#' @param u_opt_range Range of thermal optima.
#' @param u_opt_distribution Distribution for thermal optima.
#' @param sd_u_mean Mean standard-deviation width of the Gaussian uptake curve.
#' @param sd_u_range Range of standard-deviation widths for Gaussian uptake
#'   curves.
#' @param sd_u_distribution Distribution for Gaussian uptake-curve widths.
#' @param half_saturation_mean Mean Monod half-saturation constant.
#' @param half_saturation_range Range of half-saturation constants.
#' @param half_saturation_distribution Distribution for half-saturation constants.
#' @param consumer_death_rate Consumer death rate.
#' @param resource_renewal_rate Chemostat resource renewal rate.
#' @param resource_supply Resource supply concentration.
#' @param conversion_efficiency Conversion efficiency from uptake to consumer
#'   growth.
#' @param resource_use_mode Resource-use mode. One of
#'   `"one_resource_all_consumers"`, `"diagonal"`, or `"shared_to_private"`.
#' @param active_resource Active resource index for
#'   `"one_resource_all_consumers"` or shared resource index for
#'   `"shared_to_private"`.
#' @param resource_specialization Backwards-compatible scalar value between 0
#'   and 1 controlling the transition from shared-resource use to private-
#'   resource use when `resource_use_mode = "shared_to_private"`.
#' @param resource_specialization_distribution Distribution used to generate
#'   species-level shared-private partition values. One of `"constant"`,
#'   `"regular"`, `"random_uniform"`, or `"beta"`.
#' @param resource_specialization_mean Mean species-level private-resource
#'   specialization.
#' @param resource_specialization_range Range for `"regular"` and
#'   `"random_uniform"` species-level private-resource specialization.
#' @param resource_specialization_precision Precision for beta-distributed
#'   species-level private-resource specialization. Larger values produce less
#'   among-species variation around `resource_specialization_mean`.
#' @param community_seed Random seed used when generating community traits.
#'
#' @return A consumer-resource community parameter object.
#' @export
#'
#' @examples NULL
make_a_consumer_resource_community <- function(S,
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

  draw_values <- function(n, mean, range, distribution) {
    if (distribution == "random_uniform") {
      stats::runif(
        n,
        min = mean - 0.5 * range,
        max = mean + 0.5 * range
      )
    } else if (distribution == "regular") {
      seq(
        from = mean - 0.5 * range,
        to = mean + 0.5 * range,
        length.out = n
      )
    } else {
      stop("Unsupported distribution: ", distribution, call. = FALSE)
    }
  }

  draw_resource_specialization <- function(n,
                                           mean,
                                           range,
                                           distribution,
                                           precision) {
    if (distribution %in% c("constant", "regular", "random_uniform")) {
      lower <- mean - 0.5 * range
      upper <- mean + 0.5 * range
      if (lower < 0 || upper > 1) {
        stop(
          "`resource_specialization_mean +/- 0.5 * resource_specialization_range` ",
          "must stay within [0, 1].",
          call. = FALSE
        )
      }
    }

    if (distribution == "constant") {
      values <- rep(mean, n)
    } else if (distribution == "regular") {
      values <- seq(
        from = mean - 0.5 * range,
        to = mean + 0.5 * range,
        length.out = n
      )
    } else if (distribution == "random_uniform") {
      values <- stats::runif(
        n,
        min = mean - 0.5 * range,
        max = mean + 0.5 * range
      )
    } else if (distribution == "beta") {
      if (!is.numeric(mean) || length(mean) != 1 ||
          !is.finite(mean) || mean <= 0 || mean >= 1) {
        stop("Beta resource specialization requires `0 < mean < 1`.", call. = FALSE)
      }
      if (!is.numeric(precision) || length(precision) != 1 ||
          !is.finite(precision) || precision <= 0) {
        stop("Beta resource specialization requires `precision > 0`.", call. = FALSE)
      }
      values <- stats::rbeta(
        n,
        shape1 = mean * precision,
        shape2 = (1 - mean) * precision
      )
    } else {
      stop(
        "Unsupported resource specialization distribution: ",
        distribution,
        call. = FALSE
      )
    }

    values <- as.numeric(values)
    if (length(values) != n || any(!is.finite(values)) ||
        any(values < 0) || any(values > 1)) {
      stop("Species-level resource specialization values must be in [0, 1].", call. = FALSE)
    }
    values
  }

  set.seed(community_seed)

  if (resource_use_mode == "shared_to_private") {
    R <- S + 1
  } else {
    R <- S
  }

  active_resource <- as.integer(active_resource)
  if (active_resource < 1 || active_resource > R) {
    stop("`active_resource` must be between 1 and the number of resources.", call. = FALSE)
  }
  resource_specialization_i <- draw_resource_specialization(
    n = S,
    mean = resource_specialization_mean,
    range = resource_specialization_range,
    distribution = resource_specialization_distribution,
    precision = resource_specialization_precision
  )

  u_max_i <- draw_values(S, u_max_mean, u_max_range, u_max_distribution)
  u_opt_i <- draw_values(S, u_opt_mean, u_opt_range, u_opt_distribution)
  sd_u_i <- draw_values(S, sd_u_mean, sd_u_range, sd_u_distribution)

  if (any(sd_u_i <= 0)) {
    stop("All uptake breadth values must be positive.", call. = FALSE)
  }

  h_ij <- matrix(
    draw_values(S * R, half_saturation_mean, half_saturation_range, half_saturation_distribution),
    nrow = S,
    ncol = R
  )

  if (any(h_ij <= 0)) {
    stop("All half-saturation values must be positive.", call. = FALSE)
  }

  resource_use_ij <- matrix(0, nrow = S, ncol = R)
  if (resource_use_mode == "one_resource_all_consumers") {
    resource_use_ij[, active_resource] <- 1
  } else if (resource_use_mode == "diagonal") {
    diag(resource_use_ij) <- 1
  } else if (resource_use_mode == "shared_to_private") {
    private_resources <- setdiff(seq_len(R), active_resource)
    resource_use_ij[, active_resource] <- 1 - resource_specialization_i
    resource_use_ij[cbind(seq_len(S), private_resources)] <- resource_specialization_i
  } else {
    stop("Unsupported resource-use mode: ", resource_use_mode, call. = FALSE)
  }

  a_u_ij <- matrix(rep(u_max_i, times = R), nrow = S, ncol = R)
  u_opt_ij <- matrix(rep(u_opt_i, times = R), nrow = S, ncol = R)
  sd_u_ij <- matrix(rep(sd_u_i, times = R), nrow = S, ncol = R)

  d_i <- rep(consumer_death_rate, S)
  e_i <- rep(conversion_efficiency, S)
  rho_j <- rep(resource_renewal_rate, R)
  K_R_j <- rep(resource_supply, R)

  if (any(d_i < 0) || any(e_i < 0) || any(rho_j < 0) || any(K_R_j <= 0)) {
    stop("Death, efficiency, renewal, and supply parameters are outside valid ranges.", call. = FALSE)
  }

  active_u_max_i <- rowSums(resource_use_ij * a_u_ij)

  list(
    model_type = "consumer_resource",
    S = S,
    R = R,
    a_u_ij = a_u_ij,
    u_opt_ij = u_opt_ij,
    sd_u_ij = sd_u_ij,
    h_ij = h_ij,
    resource_use_ij = resource_use_ij,
    d_i = d_i,
    e_i = e_i,
    rho_j = rho_j,
    K_R_j = K_R_j,
    resource_use_mode = resource_use_mode,
    active_resource = active_resource,
    private_resource_use = resource_specialization_i,
    private_resource_use_i = resource_specialization_i,
    private_resource_use_distribution = resource_specialization_distribution,
    private_resource_use_mean = resource_specialization_mean,
    private_resource_use_range = resource_specialization_range,
    private_resource_use_precision = resource_specialization_precision,
    resource_specialization = resource_specialization_i,
    resource_specialization_i = resource_specialization_i,
    resource_specialization_distribution = resource_specialization_distribution,
    resource_specialization_mean = resource_specialization_mean,
    resource_specialization_range = resource_specialization_range,
    resource_specialization_precision = resource_specialization_precision,
    uptake_maximum_i = u_max_i,
    uptake_optimum_i = u_opt_i,
    uptake_width_i = sd_u_i,
    uptake_maximum_ij = a_u_ij,
    uptake_optimum_ij = u_opt_ij,
    uptake_width_ij = sd_u_ij,
    half_saturation_ij = h_ij,
    u_max_i = u_max_i,
    u_opt_i = u_opt_i,
    sd_u_i = sd_u_i,
    consumer_death_rate = consumer_death_rate,
    resource_renewal_rate = resource_renewal_rate,
    resource_supply = resource_supply,
    conversion_efficiency = conversion_efficiency,
    # Compatibility fields for expected consumer growth curves used in CPC summaries.
    a_b_i = e_i * active_u_max_i,
    b_opt_i = u_opt_i,
    sd_perf_i = sd_u_i,
    s_i = sd_u_i,
    a_d_i = d_i,
    z_i = rep(0, S)
  )
}
