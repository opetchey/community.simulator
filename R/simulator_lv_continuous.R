#' Simulate Lotka-Volterra community dynamics in continuous time
#'
#' The LV carrying-capacity scaling constants are fixed internally at
#' `bet = delt = 0.001`, and net growth includes a small offset of `1e-6`.
#' For a community built with `simulation.rate_scale`, this offset is also
#' multiplied by that scale to preserve exact biological time rescaling.
#'
#' @param input_com_params Community object, containing all species and
#'   community parameters.
#' @param TcelSeries Time series of temperature values.
#' @param initial_abundances Initial abundances of each species.
#' @param times Times at which temperatures are defined. Defaults to one time
#'   unit per temperature value.
#' @param output_times Times at which abundances should be returned.
#' @param temperature_interpolation How temperature should be interpolated
#'   between supplied time points. One of `"linear"` or `"constant"`.
#' @param immigration_rate Immigration rate per species.
#' @param immigration_mode One of `"continuous"` or `"pulse"`.
#' @param ode_method ODE solver method passed to `deSolve::ode()`.
#' @param rtol Relative tolerance passed to `deSolve::ode()`.
#' @param atol Absolute tolerance passed to `deSolve::ode()`.
#' @param max_step Maximum solver step size.
#' @param maxsteps Optional maximum number of internal solver steps before
#'   reaching the next output time. Passed to `deSolve::ode()` when supplied.
#' @param blowup_threshold Abundance threshold above which the run stops.
#' @param rhs RHS implementation: `"r"` (default) or `"compiled"`.
#' @param negative_tolerance Nonnegative tolerance for numerical negative
#'   returned abundances. Output below its negative causes failure; tolerated
#'   negative output is clipped to zero. Negative internal trial states are
#'   evaluated at zero, allowing the solver to recover.
#'
#' @return Time series of population abundances for each species. If negative
#'   internal trials occurred, the `ode_negative_trials` attribute records their
#'   evaluation count, minimum abundance and time of that minimum.
#' @export
#'
#' @examples NULL
simulator_lv_continuous <- function(input_com_params,
                                    TcelSeries,
                                    initial_abundances,
                                    times = seq_len(ncol(TcelSeries)),
                                    output_times = times,
                                    temperature_interpolation = "linear",
                                    immigration_rate = 0.1,
                                    immigration_mode = "continuous",
                                    ode_method = "lsoda",
                                    rtol = 1e-6,
                                    atol = 1e-8,
                                    max_step = 1,
                                    maxsteps = NULL,
                                    blowup_threshold = 1e12,
                                    negative_tolerance = 1e-8,
                                    rhs = "r") {

  rhs <- match.arg(rhs, c("r", "compiled"))

  require_nonnegative_number(negative_tolerance, "negative_tolerance")
  negative_tolerance <- as.numeric(negative_tolerance)

  temperature_interpolation <- match.arg(
    temperature_interpolation,
    choices = c("linear", "constant")
  )
  immigration_mode <- match.arg(
    immigration_mode,
    choices = c("continuous", "pulse")
  )

  S <- input_com_params$S
  al <- input_com_params$alpha_ij
  bopt <- input_com_params$b_opt_i
  spread <- input_com_params$sd_perf_i
  if (is.null(spread)) {
    spread <- input_com_params$s_i
  }
  ab <- input_com_params$a_b_i
  ad <- input_com_params$a_d_i
  z <- input_com_params$z_i

  bet <- delt <- 0.001

  temperatures <- as.numeric(TcelSeries)
  if (length(times) != length(temperatures)) {
    stop("`times` and `TcelSeries` must have the same length.", call. = FALSE)
  }
  if (length(initial_abundances) != S) {
    stop("`initial_abundances` must have one value per species.", call. = FALSE)
  }
  if (any(!is.finite(initial_abundances)) || any(initial_abundances < 0)) {
    stop("`initial_abundances` must be finite and nonnegative.", call. = FALSE)
  }

  state_names <- paste0("Spp", seq_len(S))
  state <- stats::setNames(as.numeric(initial_abundances), state_names)
  integration_times <- sort(unique(c(0, output_times)))

  temperature_at_time <- stats::approxfun(
    x = times,
    y = temperatures,
    method = temperature_interpolation,
    rule = 2,
    f = 0
  )

  negative_trial_evaluations <- 0L
  minimum_trial_abundance <- 0
  time_of_minimum_trial <- NA_real_

  derivative <- function(t, state, parms) {
    if (any(!is.finite(state))) {
      stop("Non-finite abundance encountered by ODE solver.", call. = FALSE)
    }
    if (any(state > blowup_threshold)) {
      stop("Abundance exceeded blow-up threshold.", call. = FALSE)
    }

    if (any(state < 0)) {
      negative_trial_evaluations <<- negative_trial_evaluations + 1L
      minimum_state <- min(state)
      if (minimum_state < minimum_trial_abundance) {
        minimum_trial_abundance <<- minimum_state
        time_of_minimum_trial <<- t
      }
    }
    # Evaluate trial states at the boundary without changing solver state.
    # Validate the solver's returned abundances separately below.
    Nt <- pmax(as.numeric(state), 0)
    Tcel <- temperature_at_time(t)

    b0 <- ab * exp(-0.5 * ((Tcel - bopt) / spread)^2)
    d0 <- ad * exp(z * Tcel)
    rms <- b0 - d0 + 1e-6 * (input_com_params$rate_scale %||% 1)
    K <- rms / (bet + delt)

    myrate <- rms * (1 - as.numeric(al %*% Nt) / K)
    immigration <- if (immigration_mode == "continuous") immigration_rate else 0
    dNdt <- Nt * myrate + immigration

    if (any(!is.finite(dNdt))) {
      stop("Non-finite derivative encountered by ODE solver.", call. = FALSE)
    }

    list(dNdt)
  }

  event_data <- NULL
  if (immigration_mode == "pulse" && immigration_rate != 0) {
    event_times <- output_times[output_times > min(integration_times)]
    event_data <- expand.grid(
      var = state_names,
      time = event_times,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    event_data$value <- immigration_rate
    event_data$method <- "add"
  }

  ode_args <- list(
    y = state,
    times = integration_times,
    func = derivative,
    parms = NULL,
    method = ode_method,
    rtol = rtol,
    atol = atol,
    hmax = max_step,
    events = if (is.null(event_data)) NULL else list(data = event_data)
  )
  if (!is.null(maxsteps)) {
    ode_args$maxsteps <- maxsteps
  }

  if (rhs == "compiled") {
    parameters <- c(S, 0, if (immigration_mode == "continuous") immigration_rate else 0,
      blowup_threshold, 1e-6 * (input_com_params$rate_scale %||% 1),
      as.numeric(al), bopt, spread, ab, ad, z)
    expected <- 5 + S*S + 5*S
    if (length(parameters) != expected) stop("Invalid compiled RHS parameter dimensions.")
    ode_args <- compiled_rhs_args(ode_args, "lv", parameters,
                                  times, temperatures, temperature_interpolation)
  }

  ode_output <- run_ode_with_captured_output(
    ode_args,
    failure_prefix = "Continuous-time ODE solve failed: "
  )
  if (rhs == "compiled") {
    trials <- .Call("cs_trials", PACKAGE = "community.simulator")
    negative_trial_evaluations <- as.integer(trials[1])
    minimum_trial_abundance <- trials[2]
    time_of_minimum_trial <- trials[3]
  }
  solver_output <- attr(ode_output, "community.simulator.ode_solver_output")

  output <- as.data.frame(ode_output)
  output <- output[output$time %in% output_times, state_names, drop = FALSE]
  rownames(output) <- NULL

  if (nrow(output) != length(output_times)) {
    stop(
      "ODE solver did not return all requested output times. If deSolve reported excessive work or MXSTEP, try increasing `simulation.ode.maxsteps`.",
      format_ode_solver_output(solver_output),
      call. = FALSE
    )
  }
  if (any(output < -negative_tolerance, na.rm = TRUE)) {
    stop("Continuous-time simulation returned negative abundances.", call. = FALSE)
  }
  if (any(!is.finite(as.matrix(output)))) {
    stop("Continuous-time simulation returned non-finite abundances.", call. = FALSE)
  }

  output[] <- lapply(output, function(x) pmax(x, 0))
  if (negative_trial_evaluations > 0L) {
    attr(output, "ode_negative_trials") <- list(
      negative_trial_evaluations = negative_trial_evaluations,
      minimum_trial_abundance = minimum_trial_abundance,
      time_of_minimum_trial = time_of_minimum_trial
    )
  }
  output
}
