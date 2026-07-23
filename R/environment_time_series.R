generate_one_over_f_temperature <- function(n,
                                            mean,
                                            sd,
                                            gamma) {
  n <- as.integer(n)
  target_mean <- as.numeric(mean)
  sd <- as.numeric(sd)
  gamma <- as.numeric(gamma)

  if (length(n) != 1 || is.na(n) || n < 2) {
    stop("`n` must be an integer >= 2.", call. = FALSE)
  }
  if (length(target_mean) != 1 || is.na(target_mean) || !is.finite(target_mean)) {
    stop("`mean` must be one finite number.", call. = FALSE)
  }
  if (length(sd) != 1 || is.na(sd) || !is.finite(sd) || sd < 0) {
    stop("`sd` must be one non-negative finite number.", call. = FALSE)
  }
  if (length(gamma) != 1 || is.na(gamma) || !is.finite(gamma)) {
    stop("`gamma` must be one finite number.", call. = FALSE)
  }

  if (sd == 0) {
    return(rep(target_mean, n))
  }

  spectrum <- complex(length.out = n)
  max_frequency <- floor((n - 1L) / 2L)
  if (max_frequency >= 1L) {
    frequencies <- seq_len(max_frequency)
    amplitudes <- frequencies^(-gamma)
    phases <- stats::runif(max_frequency, min = 0, max = 2 * pi)
    coefficients <- amplitudes * complex(real = cos(phases), imaginary = sin(phases))
    positive_indices <- frequencies + 1L
    negative_indices <- n - frequencies + 1L
    spectrum[positive_indices] <- coefficients
    spectrum[negative_indices] <- Conj(coefficients)
  }

  if (n %% 2L == 0L) {
    nyquist_frequency <- n / 2L
    spectrum[nyquist_frequency + 1L] <- nyquist_frequency^(-gamma) *
      sample(c(-1, 1), size = 1)
  }

  raw <- Re(stats::fft(spectrum, inverse = TRUE))
  raw_sd <- stats::sd(raw)
  if (!is.finite(raw_sd) || raw_sd == 0) {
    return(rep(target_mean, n))
  }

  ((raw - base::mean(raw)) / raw_sd) * sd + target_mean
}
