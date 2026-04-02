#' Get the sum of the communities derivatives at arbitrary temperatures and at actual (temp = temporal) temperatures
#'
#' @param arb_derivs Connection to database containing arbitrary derivatives
#' @param temp_derivs Connection to database containing temporal (actual) derivatives
#' @param delta_igr Connection to database containing delta IGR
#'
#' @return A dataset containing the sum of the derivatives at arbitrary and actual temperatures
#' @export
#'
#' @examples NULL
get_community_sum_derivatives <- function(arb_derivs, temp_derivs, delta_igr) {

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
  temp1 <- arb_derivs |>
    dplyr::group_by(case_id, temperature) |>
    dplyr::summarise(sum_arb_deriv = sum(derivative)) |>
    dplyr::collect()
  sum2_derivs_arb <- temp1 |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(mean_arb_deriv = mean(abs(sum_arb_deriv)))

  #ggplot(temp1, aes(x = sum_arb_deriv)) +
  #  geom_histogram() +
  #  facet_wrap(~case_id)
  #ggplot(temp1, aes(x = temperature, y = sum_arb_deriv)) +
  #  geom_point() +
  #  facet_wrap(~case_id)



  ### Actual / temporal
  temp1 <- temp_derivs |>
    dplyr::group_by(case_id, temperature) |>
    dplyr::summarise(sum_temp_deriv = sum(derivative)) |>
    dplyr::collect()
  sum2_derivs_temp <- temp1 |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(mean_temp_deriv = mean(abs(sum_temp_deriv)))

  #ggplot(temp1, aes(x = sum_temp_deriv)) +
  #  geom_histogram() +
  #  facet_wrap(~case_id)
  #ggplot(temp1, aes(x = temperature, y = sum_temp_deriv)) +
  #  geom_point() +
  #  facet_wrap(~case_id)


  # delta_igr
  temp1 <- delta_igr |>
    dplyr::group_by(case_id, time) |>
    dplyr::summarise(sum_delta_igr = sum(delta_igr)) |>
    dplyr::collect()
  sum2_delta_igr <- temp1 |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(mean_delta_igr = mean(abs(sum_delta_igr), na.rm = TRUE))



  sum_derivs <- dplyr::full_join(sum2_derivs_arb, sum2_derivs_temp) |>
    dplyr::full_join(sum2_delta_igr)
  return(sum_derivs)

}
