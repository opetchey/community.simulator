make_lv_interaction_matrix <- function(S, spec) {
  spec <- as.list(spec)
  interaction_type <- spec$type %||% "competition"
  symmetry <- spec$symmetry %||% "asymmetric"
  distribution <- spec$distribution %||% "constant"
  parameters <- as.list(spec$parameters %||% list(value = 0))
  diagonal <- spec$diagonal %||% spec$alpha_jj %||% 1

  interaction_type <- match.arg(
    interaction_type,
    choices = c("none", "competition", "any", "predator_prey")
  )
  symmetry <- match.arg(
    symmetry,
    choices = c("asymmetric", "symmetric", "antisymmetric")
  )
  distribution <- match.arg(
    distribution,
    choices = c("constant", "uniform", "normal", "lognormal", "gamma")
  )

  if (interaction_type == "none") {
    alpha_ij <- matrix(0, nrow = S, ncol = S)
    diag(alpha_ij) <- diagonal
    return(alpha_ij)
  }

  if (interaction_type == "predator_prey" && symmetry == "symmetric") {
    stop("`predator_prey` interactions cannot use `symmetric` symmetry.", call. = FALSE)
  }
  if (interaction_type %in% c("competition", "any") && symmetry == "antisymmetric") {
    stop("`antisymmetric` symmetry is only supported for `predator_prey` interactions.", call. = FALSE)
  }

  draw_values <- function(n) {
    values <- switch(
      distribution,
      constant = {
        value <- parameters$value %||% parameters$magnitude %||% parameters$mean %||% 0
        rep(value, n)
      },
      uniform = {
        min_value <- parameters$min %||% parameters$lower %||% parameters$min_abs
        max_value <- parameters$max %||% parameters$upper %||% parameters$max_abs
        if (is.null(min_value) || is.null(max_value)) {
          stop("Uniform interactions require `min` and `max` parameters.", call. = FALSE)
        }
        stats::runif(n, min = min_value, max = max_value)
      },
      normal = {
        mean_value <- parameters$mean %||% 0
        sd_value <- parameters$sd
        if (is.null(sd_value)) {
          stop("Normal interactions require an `sd` parameter.", call. = FALSE)
        }
        stats::rnorm(n, mean = mean_value, sd = sd_value)
      },
      lognormal = {
        meanlog_value <- parameters$meanlog %||% 0
        sdlog_value <- parameters$sdlog
        if (is.null(sdlog_value)) {
          stop("Lognormal interactions require an `sdlog` parameter.", call. = FALSE)
        }
        stats::rlnorm(n, meanlog = meanlog_value, sdlog = sdlog_value)
      },
      gamma = {
        shape_value <- parameters$shape
        if (is.null(shape_value)) {
          stop("Gamma interactions require a `shape` parameter.", call. = FALSE)
        }
        if (!is.null(parameters$rate)) {
          stats::rgamma(n, shape = shape_value, rate = parameters$rate)
        } else if (!is.null(parameters$scale)) {
          stats::rgamma(n, shape = shape_value, scale = parameters$scale)
        } else {
          stop("Gamma interactions require either `rate` or `scale`.", call. = FALSE)
        }
      }
    )
    as.numeric(values)
  }

  validate_competition_parameters <- function() {
    if (distribution == "constant") {
      value <- parameters$value %||% parameters$mean %||% 0
      if (value < 0) {
        stop("Competition interactions require non-negative constant values.", call. = FALSE)
      }
    }
    if (distribution == "uniform") {
      min_value <- parameters$min %||% parameters$lower
      if (is.null(min_value) || min_value < 0) {
        stop("Uniform competition interactions require `min >= 0`.", call. = FALSE)
      }
    }
    if (distribution == "normal") {
      stop(
        "Normal competition interactions are not supported because they can be negative. ",
        "Use `uniform`, `lognormal`, `gamma`, or `constant`.",
        call. = FALSE
      )
    }
  }

  if (interaction_type == "competition") {
    validate_competition_parameters()
  }

  alpha_ij <- matrix(0, nrow = S, ncol = S)

  if (interaction_type == "predator_prey") {
    pair_indices <- utils::combn(seq_len(S), 2)
    first_magnitudes <- abs(draw_values(ncol(pair_indices)))
    second_magnitudes <- if (symmetry == "antisymmetric") {
      first_magnitudes
    } else {
      abs(draw_values(ncol(pair_indices)))
    }
    directions <- sample(c(-1, 1), ncol(pair_indices), replace = TRUE)

    for (pair_index in seq_len(ncol(pair_indices))) {
      i <- pair_indices[1, pair_index]
      j <- pair_indices[2, pair_index]
      alpha_ij[i, j] <- directions[[pair_index]] * first_magnitudes[[pair_index]]
      alpha_ij[j, i] <- -directions[[pair_index]] * second_magnitudes[[pair_index]]
    }
  } else if (symmetry == "asymmetric") {
    off_diagonal <- row(alpha_ij) != col(alpha_ij)
    alpha_ij[off_diagonal] <- draw_values(sum(off_diagonal))
  } else {
    pair_indices <- utils::combn(seq_len(S), 2)
    pair_values <- draw_values(ncol(pair_indices))
    for (pair_index in seq_len(ncol(pair_indices))) {
      i <- pair_indices[1, pair_index]
      j <- pair_indices[2, pair_index]
      alpha_ij[i, j] <- pair_values[[pair_index]]
      alpha_ij[j, i] <- if (symmetry == "symmetric") {
        pair_values[[pair_index]]
      } else {
        -pair_values[[pair_index]]
      }
    }
  }

  if (interaction_type == "competition" && any(alpha_ij[row(alpha_ij) != col(alpha_ij)] < 0)) {
    stop("Competition interactions must be non-negative.", call. = FALSE)
  }
  if (interaction_type == "predator_prey") {
    off_diagonal_pairs <- utils::combn(seq_len(S), 2)
    pair_products <- apply(off_diagonal_pairs, 2, function(pair) {
      alpha_ij[pair[1], pair[2]] * alpha_ij[pair[2], pair[1]]
    })
    if (any(pair_products > 0)) {
      stop("Predator-prey interaction pairs must have opposite signs.", call. = FALSE)
    }
  }

  diag(alpha_ij) <- diagonal
  alpha_ij
}

make_legacy_lv_interaction_spec <- function(alpha_ij_mean,
                                            alpha_ij_sd,
                                            alpha_jj,
                                            alpha_ij_distribution) {
  if (alpha_ij_sd == 0 && alpha_ij_mean == 0) {
    return(list(
      type = "none",
      diagonal = alpha_jj,
      label = "legacy_none"
    ))
  }

  if (alpha_ij_distribution == "random_uniform") {
    return(list(
      type = if (alpha_ij_mean - 0.5 * alpha_ij_sd >= 0) "competition" else "any",
      symmetry = "asymmetric",
      distribution = "uniform",
      parameters = list(
        min = alpha_ij_mean - 0.5 * alpha_ij_sd,
        max = alpha_ij_mean + 0.5 * alpha_ij_sd
      ),
      diagonal = alpha_jj,
      label = "legacy_uniform"
    ))
  }

  if (alpha_ij_distribution == "random_normal") {
    return(list(
      type = "any",
      symmetry = "asymmetric",
      distribution = "normal",
      parameters = list(
        mean = alpha_ij_mean,
        sd = alpha_ij_sd
      ),
      diagonal = alpha_jj,
      label = "legacy_normal"
    ))
  }

  stop("Unsupported legacy interaction distribution: ", alpha_ij_distribution, call. = FALSE)
}

#' Make a community parameter object
#'
#' This function makes a community object containing the parameters for all
#' species in the community. At the moment, only the temperature optima
#' `b_opt_i` vary among species; the remaining parameters are shared or are
#' generated from common distributions.
#'
#' @param S Number of species in the community
#' @param a_b_mean Mean of the distribution from which a_b values are drawn.
#' @param a_b_range Range of the distribution from which a_b values are drawn.
#' @param a_b_distribution Distribution used to generate `a_b_i` values.
#' @param b_opt_mean Mean of the distribution from which `b_opt_i` values are drawn.
#' @param b_opt_range Range of the distribution from which `b_opt_i` values are drawn.
#' @param b_opt_distribution Distribution used to generate `b_opt_i` values.
#' @param sd_perf_distribution Distribution used to generate Gaussian
#'   performance-curve widths.
#' @param sd_perf_mean Mean of the distribution from which Gaussian
#'   performance-curve widths are drawn.
#' @param sd_perf_range Range of the distribution from which Gaussian
#'   performance-curve widths are drawn.
#' @param community_seed Random seed used when generating community traits.
#' @param a_d Death rate when temperature is equal to 0; same for all species.
#' @param z Exponential rate of increase in death rate with temperature; same for all species.
#' @param alpha_ij_mean Deprecated. Mean used by the legacy interaction
#'   generator.
#' @param alpha_ij_sd Deprecated. Spread used by the legacy interaction
#'   generator.
#' @param alpha_jj Value of the diagonal of the community matrix, shared across species.
#' @param alpha_ij_distribution Deprecated. Distribution used by the legacy
#'   interaction generator.
#' @param lv_interaction_spec Optional named list specifying LV interactions.
#'   Preferred over legacy `alpha_ij_*` arguments. Fields include `type`
#'   (`"none"`, `"competition"`, `"any"`, or `"predator_prey"`), `symmetry`
#'   (`"asymmetric"`, `"symmetric"`, or `"antisymmetric"`), `distribution`
#'   (`"constant"`, `"uniform"`, `"normal"`, `"lognormal"`, or `"gamma"`),
#'   `parameters`, and `diagonal`.
#'
#' @return Returns a list containing the community object.
#' @export
#'
#' @examples NULL
make_a_community <- function(S,

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
                             lv_interaction_spec = NULL){

  ## same a_b_i for all species
  #a_b_i <- rep(a_b, S)

  set.seed(community_seed)

  ## a_b values according to trait_selection_method
  if(a_b_distribution == "random_uniform") {
    #set.seed(a_b_realisation_seed)
    a_b_i <- stats::runif(S, min= a_b_mean - (0.5*a_b_range),
                          max = a_b_mean + (0.5*a_b_range))
  }
  if(a_b_distribution == "regular") {
    #set.seed(a_b_realisation_seed)
    a_b_i <- seq(length.out = S, from = a_b_mean - (0.5*a_b_range),
                   to = a_b_mean + (0.5*a_b_range))
  }

  ## b_opt values according to trait_selection_method
  if(b_opt_distribution == "random_uniform") {
    #set.seed(b_opt_realisation_seed)
    b_opt_i <- stats::runif(S, min= b_opt_mean - (0.5*b_opt_range),
                            max = b_opt_mean + (0.5*b_opt_range))
  }
  if(b_opt_distribution == "regular") {
    #set.seed(b_opt_realisation_seed)
    b_opt_i <- seq(length.out = S, from = b_opt_mean - (0.5*b_opt_range),
                 to = b_opt_mean + (0.5*b_opt_range))
  }



  ## standard-deviation width of the Gaussian birth rate - temperature response curve
  ## not same for all species
  if(sd_perf_distribution == "random_uniform") {
    #set.seed(a_b_realisation_seed)
    sd_perf_i <- stats::runif(S, min= sd_perf_mean - (0.5*sd_perf_range),
                              max = sd_perf_mean + (0.5*sd_perf_range))
  }
  if(sd_perf_distribution == "regular") {
    sd_perf_i <- seq(length.out = S, from = sd_perf_mean - (0.5*sd_perf_range),
                     to = sd_perf_mean + (0.5*sd_perf_range))
  }
  if(!sd_perf_distribution %in% c("random_uniform", "regular")) {
    stop("Unsupported performance-curve width distribution: ", sd_perf_distribution, call. = FALSE)
  }
  if (any(sd_perf_i <= 0)) {
    stop("All performance-curve width values must be positive.", call. = FALSE)
  }


  ## death rate when temperature is equal to 0
  ## same for all species
  a_d_i <- rep(a_d, S)

  ## exponential rate of increase in death rate with temperature
  z_i <- rep(z, S)

  if (is.null(lv_interaction_spec)) {
    lv_interaction_spec <- make_legacy_lv_interaction_spec(
      alpha_ij_mean = alpha_ij_mean,
      alpha_ij_sd = alpha_ij_sd,
      alpha_jj = alpha_jj,
      alpha_ij_distribution = alpha_ij_distribution
    )
  }

  alpha_ij <- make_lv_interaction_matrix(S, lv_interaction_spec)

  ## create a list of all the parameters
  community_pars_object <- list(S = S,
                                a_b_i = a_b_i,
                                b_opt_i = b_opt_i,
                                sd_perf_i = sd_perf_i,
                                s_i = sd_perf_i,
                                a_d_i = a_d_i,
                                z_i = z_i,
                                alpha_ij = alpha_ij,
                                lv_interaction_spec = lv_interaction_spec)

  community_pars_object
}
