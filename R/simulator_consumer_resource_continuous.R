#' Simulate consumer-resource dynamics in continuous time
#'
#' @param input_com_params Consumer-resource community object.
#' @param TcelSeries Time series of temperature values.
#' @param initial_consumer_abundances Initial consumer abundances.
#' @param initial_resource_values Initial resource values.
#' @param times Times at which temperatures are defined.
#' @param output_times Times at which states should be returned.
#' @param temperature_interpolation Temperature interpolation method.
#' @param consumer_immigration_rate Consumer immigration rate per unit time.
#' @param ode_method ODE solver method passed to `deSolve::ode()`.
#' @param rtol Relative tolerance passed to `deSolve::ode()`.
#' @param atol Absolute tolerance passed to `deSolve::ode()`.
#' @param max_step Maximum solver step size.
#' @param blowup_threshold State-value threshold above which the run stops.
#' @param negative_tolerance Negative tolerance for numerical error.
#'
#' @return A named list with `consumers` and `resources` data frames.
#' @export
#'
#' @examples NULL
simulator_consumer_resource_continuous <- function(input_com_params,
                                                   TcelSeries,
                                                   initial_consumer_abundances,
                                                   initial_resource_values,
                                                   times = seq_len(ncol(TcelSeries)),
                                                   output_times = times,
                                                   temperature_interpolation = "linear",
                                                   consumer_immigration_rate = 0.1,
                                                   ode_method = "lsoda",
                                                   rtol = 1e-6,
                                                   atol = 1e-8,
                                                   max_step = 1,
                                                   blowup_threshold = 1e12,
                                                   negative_tolerance = 1e-8) {

  temperature_interpolation <- match.arg(
    temperature_interpolation,
    choices = c("linear", "constant")
  )

  S <- input_com_params$S
  R <- input_com_params$R
  temperatures <- as.numeric(TcelSeries)

  if (length(times) != length(temperatures)) {
    stop("`times` and `TcelSeries` must have the same length.", call. = FALSE)
  }
  if (length(initial_consumer_abundances) != S) {
    stop("`initial_consumer_abundances` must have one value per consumer.", call. = FALSE)
  }
  if (length(initial_resource_values) != R) {
    stop("`initial_resource_values` must have one value per resource.", call. = FALSE)
  }

  consumer_names <- paste0("Spp", seq_len(S))
  resource_names <- paste0("Res", seq_len(R))
  state <- stats::setNames(
    c(as.numeric(initial_consumer_abundances), as.numeric(initial_resource_values)),
    c(consumer_names, resource_names)
  )

  integration_times <- sort(unique(c(min(times) - 1, output_times)))
  temperature_at_time <- stats::approxfun(
    x = times,
    y = temperatures,
    method = temperature_interpolation,
    rule = 2,
    f = 0
  )

  derivative <- function(t, state, parms) {
    if (any(!is.finite(state))) {
      stop("Non-finite state encountered by ODE solver.", call. = FALSE)
    }
    if (any(state > blowup_threshold)) {
      stop("State exceeded blow-up threshold.", call. = FALSE)
    }

    # ODE solvers can briefly probe negative states near zero. Evaluate the
    # vector field on the biologically meaningful boundary and validate the
    # returned output after integration.
    N <- pmax(as.numeric(state[consumer_names]), 0)
    resources <- pmax(as.numeric(state[resource_names]), 0)
    Tcel <- temperature_at_time(t)

    u_max_ij <- input_com_params$a_u_ij *
      exp(-0.5 * ((Tcel - input_com_params$u_opt_ij) / input_com_params$sd_u_ij)^2)
    uptake_ij <- input_com_params$resource_use_ij *
      u_max_ij *
      matrix(resources, nrow = S, ncol = R, byrow = TRUE) /
      (input_com_params$h_ij + matrix(resources, nrow = S, ncol = R, byrow = TRUE))

    consumer_growth <- input_com_params$e_i * rowSums(uptake_ij) - input_com_params$d_i
    dNdt <- N * consumer_growth + consumer_immigration_rate
    dRdt <- input_com_params$rho_j * (input_com_params$K_R_j - resources) -
      colSums(matrix(N, nrow = S, ncol = R) * uptake_ij)

    derivs <- c(dNdt, dRdt)
    if (any(!is.finite(derivs))) {
      stop("Non-finite derivative encountered by ODE solver.", call. = FALSE)
    }

    list(derivs)
  }

  ode_output <- tryCatch(
    deSolve::ode(
      y = state,
      times = integration_times,
      func = derivative,
      parms = NULL,
      method = ode_method,
      rtol = rtol,
      atol = atol,
      hmax = max_step
    ),
    error = function(e) {
      stop("Consumer-resource ODE solve failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  output <- as.data.frame(ode_output)
  output <- output[output$time %in% output_times, , drop = FALSE]
  rownames(output) <- NULL

  if (nrow(output) != length(output_times)) {
    stop("ODE solver did not return all requested output times.", call. = FALSE)
  }
  state_matrix <- as.matrix(output[, c(consumer_names, resource_names), drop = FALSE])
  if (any(state_matrix < -negative_tolerance, na.rm = TRUE)) {
    stop("Consumer-resource simulation returned negative states.", call. = FALSE)
  }
  if (any(!is.finite(state_matrix))) {
    stop("Consumer-resource simulation returned non-finite states.", call. = FALSE)
  }

  output[, c(consumer_names, resource_names)] <- lapply(
    output[, c(consumer_names, resource_names), drop = FALSE],
    function(x) pmax(x, 0)
  )

  consumers <- output[, consumer_names, drop = FALSE]
  resources <- output[, resource_names, drop = FALSE]

  list(
    consumers = consumers,
    resources = resources
  )
}
