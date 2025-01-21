
intrinsic_growth_rate <- function(species_pars,
                                  temperature)
  {
  b0 <- species_pars$a_b_i * exp(-(temperature - species_pars$b_opt_i)^2 / species_pars$s_i)
  d0 <- species_pars$a_d_i * exp(species_pars$z_i * temperature)
  b0 - d0
}


intrinsic_growth_rate2 <- function(a_b_i,
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
