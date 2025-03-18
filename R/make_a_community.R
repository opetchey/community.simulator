#' This function makes a community object, which contains all the parameters of the all speices in the community. At the moment only the temperature optima $b_{opt_i}$ can vary among the species. The other parameters are the same for all species.
#'
#' @param S Number of species in the community
#' @param a_b Birth rate when temperature is equal to b_opt_i; same for all species.
#' @param b_opt_mean Mean of the distribution from which b_opt_i values are drawn.
#' @param b_opt_range Range of the distribution from which b_opt_i values are drawn.
#' @param a_b_mean Mean of the distribution from which a_b values are drawn.
#' @param a_b_range Range of the distribution from which a_b values are drawn.
#' @param s Standard deviation of the Gaussian birth rate - temperature response curve; same for all species.
#' @param a_d Death rate when temperature is equal to 0; same for all species.
#' @param z Exponential rate of increase in death rate with temperature; same for all species.
#' @param alpha_ij_mean Mean of the distribution from which the off-diagonal elements of the community matrix are drawn.
#' @param alpha_ij_sd Standard deviation of the distribution from which the off-diagonal elements of the community matrix are drawn.
#' @param alpha_jj value of the diagonal of the community matrix, same for all jj
#' @param trait_selection_method Either "random1" or "deterministic". If "random1", the b_opt_i values are drawn at random from a uniform distribution with mean b_opt_mean and range b_opt_range. If "deterministic", the b_opt_i values are equally spaced between b_opt_mean - 0.5*b_opt_range and b_opt_mean + 0.5*b_opt_range.
#'
#' @return Returns a list containing the community object.
#' @export
#'
#' @examples NULL
make_a_community <- function(S,

                             a_b_mean,
                             a_b_range,
                             a_b_distribution,
                             a_b_realisation_seed,

                             b_opt_mean,
                             b_opt_range,
                             b_opt_distribution,
                             b_opt_realisation_seed,

                             alpha_ij_mean,
                             alpha_ij_sd,
                             alpha_ij_realisation_seed,

                             s,
                             a_d,
                             z,
                             alpha_jj){

  ## same a_b_i for all species
  #a_b_i <- rep(a_b, S)

  ## a_b values according to trait_selection_method
  if(a_b_distribution == "random_uniform") {
    set.seed(a_b_realisation_seed)
    a_b_i <- runif(S, min= a_b_mean - (0.5*a_b_range),
                     max = a_b_mean + (0.5*a_b_range))
  }
  if(a_b_distribution == "regular") {
    #set.seed(a_b_realisation_seed)
    a_b_i <- seq(length.out = S, from = a_b_mean - (0.5*a_b_range),
                   to = a_b_mean + (0.5*a_b_range))
  }

  ## b_opt values according to trait_selection_method
  if(b_opt_distribution == "random_uniform") {
    set.seed(b_opt_realisation_seed)
    b_opt_i <- runif(S, min= b_opt_mean - (0.5*b_opt_range),
                   max = b_opt_mean + (0.5*b_opt_range))
  }
  if(b_opt_distribution == "regular") {
    #set.seed(b_opt_realisation_seed)
    b_opt_i <- seq(length.out = S, from = b_opt_mean - (0.5*b_opt_range),
                 to = b_opt_mean + (0.5*b_opt_range))
  }



  ## standard deviation of the Gaussian birth rate - temperature response curve
  ## same for all species
  s_i <- rep(s, S)

  ## death rate when temperature is equal to 0
  ## same for all species
  a_d_i <- rep(a_d, S)

  ## exponential rate of increase in death rate with temperature
  z_i <- rep(z, S)

  ## community matrix
  set.seed(alpha_ij_realisation_seed)
  temp <- rnorm(S*S,
                mean = alpha_ij_mean,
                sd = alpha_ij_sd)
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
