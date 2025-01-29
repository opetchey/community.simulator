#' Get the sum of the communities derivatives at arbitrary temperatures and at actual (temp = temporal) temperatures
#'
#' @param arb_derivs Connection to database containing arbitrary derivatives
#' @param temp_derivs Connection to database containing temporal (actual) derivatives
#'
#' @return A dataset containing the sum of the derivatives at arbitrary and actual temperatures
#' @export
#'
#' @examples NULL
get_community_sum_derivatives <- function(arb_derivs, temp_derivs) {

  ## get derivative of each species at mean temperature
  #spp_derivs_temp1 <- arb_derivs |>
  #  collect()
  #spp_derivs_temp2 <- spp_derivs_temp1 |>
  #  full_join(mean_temps) |>
  #  mutate(temperature_diff = abs(temperature - mean_temperature)) |>
  #  group_by(case_id, species_id) |>
  #  filter(temperature_diff == min(temperature_diff)) |>
  #  full_join(rel_b_opt)

  ## get sum derivative of each species
  sum_derivs_arb <- arb_derivs |>
    group_by(case_id) |>
    summarise(sum_arb_deriv = sum(derivative)) |>
    collect()
  sum_derivs_temp <- temp_derivs |>
    group_by(case_id) |>
    summarise(sum_temp_deriv = sum(derivative)) |>
    collect()
  sum_derivs <- full_join(sum_derivs_arb, sum_derivs_temp)
  return(sum_derivs)

}
