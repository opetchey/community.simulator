simulation_rate_scale <- function(simulation) {
  if (!"rate_scale" %in% names(simulation)) return(1)
  value <- simulation$rate_scale
  if (!is.numeric(value) || length(value) != 1L ||
      is.na(value) || !is.finite(value) || value <= 0) {
    stop("`simulation.rate_scale` must be one finite positive number.", call. = FALSE)
  }
  value
}

scale_community_rates <- function(community, scale, model_type) {
  if (scale == 1) return(community)
  fields <- if (identical(model_type, "lv_continuous")) {
    c("a_b_i", "birth_maximum_i", "a_d_i", "death_intercept_i", "alpha_ij")
  } else {
    c("a_u_ij", "uptake_maximum_ij", "u_max_i", "uptake_maximum_i",
      "d_i", "consumer_death_rate", "rho_j", "resource_renewal_rate",
      "a_b_i", "a_d_i")
  }
  for (field in fields) community[[field]] <- community[[field]] * scale
  community$rate_scale <- scale
  community
}
