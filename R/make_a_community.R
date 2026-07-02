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
#' @param sd_perf_distribution Distribution used to generate `s_i` values.
#' @param sd_perf_mean Mean of the distribution from which `s_i` values are drawn.
#' @param sd_perf_range Range of the distribution from which `s_i` values are drawn.
#' @param community_seed Random seed used when generating community traits.
#' @param s Standard deviation of the Gaussian birth rate - temperature response curve; same for all species.
#' @param a_d Death rate when temperature is equal to 0; same for all species.
#' @param z Exponential rate of increase in death rate with temperature; same for all species.
#' @param alpha_ij_mean Mean of the distribution from which the off-diagonal elements of the community matrix are drawn.
#' @param alpha_ij_sd Standard deviation of the distribution from which the off-diagonal elements of the community matrix are drawn.
#' @param alpha_jj Value of the diagonal of the community matrix, shared across species.
#' @param alpha_ij_distribution Distribution used to generate off-diagonal interaction coefficients.
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

                             alpha_ij_mean,
                             alpha_ij_sd,

                             community_seed,

                             s,
                             a_d,
                             z,
                             alpha_jj,
                             alpha_ij_distribution){

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



  ## standard deviation of the Gaussian birth rate - temperature response curve
  ## not same for all species
  if(sd_perf_distribution == "random_uniform") {
    #set.seed(a_b_realisation_seed)
    s_i <- stats::runif(S, min= sd_perf_mean - (0.5*sd_perf_range),
                        max = sd_perf_mean + (0.5*sd_perf_range))
  }


  ## death rate when temperature is equal to 0
  ## same for all species
  a_d_i <- rep(a_d, S)

  ## exponential rate of increase in death rate with temperature
  z_i <- rep(z, S)

  ## community matrix
  #set.seed(alpha_ij_realisation_seed)
  if(alpha_ij_distribution == "random_normal") {
    temp <- stats::rnorm(S*S,
                         mean = alpha_ij_mean,
                         sd = alpha_ij_sd)
  }
  if(alpha_ij_distribution == "random_uniform") {
    temp <- stats::runif(S * S,
                         min = alpha_ij_mean-(0.5*alpha_ij_sd),
                         max = alpha_ij_mean+(0.5*alpha_ij_sd))
  }
  alpha_ij <- matrix(temp, S, S)
  diag(alpha_ij) <- alpha_jj

  ## create a list of all the parameters
  community_pars_object <- list(S = S,
                                a_b_i = a_b_i,
                                b_opt_i = b_opt_i,
                                s_i = s_i,
                                a_d_i = a_d_i,
                                z_i = z_i,
                                alpha_ij = alpha_ij)

  community_pars_object
}
