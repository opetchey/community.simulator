make_a_community <- function(S,
                             a_b,
                             b_opt_mean,
                             b_opt_range,
                             s,
                             a_d,
                             z,
                             alpha_ij_mean,
                             alpha_ij_sd,
                             intrafactor,
                             trait_selection_method){
  
  a_b_i <- rep(a_b, S)
  
  if(trait_selection_method == "random1")
    b_opt_i <- runif(S, min= b_opt_mean - (0.5*b_opt_range), 
                   max = b_opt_mean + (0.5*b_opt_range))
  if(trait_selection_method == "deterministic")
    b_opt_i <- seq(length.out = S, from = b_opt_mean - (0.5*b_opt_range), 
                     to = b_opt_mean + (0.5*b_opt_range))
  
  
  s_i <- rep(s, S)
  
  a_d_i <- rep(a_d, S)
  
  z_i <- rep(z, S)
  
  temp <- rnorm(S*S,
                mean = alpha_ij_mean,
                sd = alpha_ij_sd)
  alpha_ij <- matrix(temp, S, S)
  diag(alpha_ij) <-1 # see the change - I put the diag term intra specific competition 1 so that it would be always higher than the interspecific competition
  diag(alpha_ij) <- diag(alpha_ij) * intrafactor # intrafactor currently set as 1
  
  community_pars_object <- list(S = S,
                                a_b_i = a_b_i,
                                b_opt_i = b_opt_i,
                                s_i = s_i,
                                a_d_i = a_d_i,
                                z_i = z_i,
                                alpha_ij = alpha_ij)
  
  community_pars_object
}