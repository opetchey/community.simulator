#' Simulate Lotka-Volterra community dynamics in continuous time
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
#' @param blowup_threshold Abundance threshold above which the run stops.
#'
#' @return Time series of population abundances for each species.
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
                                    blowup_threshold = 1e12) {

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

  state_names <- paste0("Spp", seq_len(S))
  state <- stats::setNames(as.numeric(initial_abundances), state_names)
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
      stop("Non-finite abundance encountered by ODE solver.", call. = FALSE)
    }
    if (any(state < -sqrt(.Machine$double.eps))) {
      stop("Negative abundance encountered by ODE solver.", call. = FALSE)
    }
    if (any(state > blowup_threshold)) {
      stop("Abundance exceeded blow-up threshold.", call. = FALSE)
    }

    Nt <- pmax(as.numeric(state), 0)
    Tcel <- temperature_at_time(t)

    b0 <- ab * exp(-0.5 * ((Tcel - bopt) / spread)^2)
    d0 <- ad * exp(z * Tcel)
    rms <- b0 - d0 + 1e-6
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

  ode_output <- tryCatch(
    deSolve::ode(
      y = state,
      times = integration_times,
      func = derivative,
      parms = NULL,
      method = ode_method,
      rtol = rtol,
      atol = atol,
      hmax = max_step,
      events = if (is.null(event_data)) NULL else list(data = event_data)
    ),
    error = function(e) {
      stop("Continuous-time ODE solve failed: ", conditionMessage(e), call. = FALSE)
    }
  )

  output <- as.data.frame(ode_output)
  output <- output[output$time %in% output_times, state_names, drop = FALSE]
  rownames(output) <- NULL

  if (nrow(output) != length(output_times)) {
    stop("ODE solver did not return all requested output times.", call. = FALSE)
  }
  if (any(output < -sqrt(.Machine$double.eps), na.rm = TRUE)) {
    stop("Continuous-time simulation returned negative abundances.", call. = FALSE)
  }
  if (any(!is.finite(as.matrix(output)))) {
    stop("Continuous-time simulation returned non-finite abundances.", call. = FALSE)
  }

  output[] <- lapply(output, function(x) pmax(x, 0))
  output
}
