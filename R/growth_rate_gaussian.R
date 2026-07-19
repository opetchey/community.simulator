#' Calculate intrinsic growth rate from species parameters and temperature
#'
#' Assumes a Gaussian birth-rate temperature response curve and an exponential
#' death-rate temperature response curve.
#'
#' @param birth_maximum Value of birth rate at the birth optimum.
#' @param birth_optimum Temperature at which birth rate is maximized.
#' @param birth_width Standard-deviation width of the Gaussian birth-rate curve.
#' @param death_intercept Value of death rate when temperature is equal to 0.
#' @param death_temperature_slope Slope of the exponential death-rate curve.
#' @param temperature Temperature at which to calculate the intrinsic growth
#'   rate.
#'
#' @return Returns the intrinsic growth rate at the given temperature
#' @export
#'
#' @examples NULL
intrinsic_growth_gaussian <- function(birth_maximum,
                                      birth_optimum,
                                      birth_width,
                                      death_intercept,
                                      death_temperature_slope,
                                      temperature)
{
  b0 <- birth_maximum * exp(-0.5 * ((temperature - birth_optimum) / birth_width)^2)
  d0 <- death_intercept * exp(death_temperature_slope * temperature)
  b0 - d0

}
