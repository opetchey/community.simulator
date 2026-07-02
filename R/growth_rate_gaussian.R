#' Calculate intrinsic growth rate from species parameters and a temperature, assuming a Gaussian birth rate - temperature response curve and an exponential death rate - temperature response curve.
#'
#' @param a_b_i Value of birth rate when temperature is equal to b_opt_i
#' @param b_opt_i Temperature at which birth rate is maximized
#' @param s_i Spread of the Gaussian curve
#' @param a_d_i Value of death rate when temperature is equal to 0
#' @param z_i Slope of the exponential curve
#' @param temperature Temperature at which to calculate the intrinsic growth rate
#'
#' @return Returns the intrinsic growth rate at the given temperature
#' @export
#'
#' @examples NULL
intrinsic_growth_gaussian <- function(a_b_i,
                                   b_opt_i,
                                   s_i,
                                   a_d_i,
                                   z_i,
                                   temperature)
{
  b0 <- a_b_i * exp(-(temperature - b_opt_i)^2 / s_i)
  d0 <- a_d_i * exp(z_i * temperature)
  b0 - d0

}
