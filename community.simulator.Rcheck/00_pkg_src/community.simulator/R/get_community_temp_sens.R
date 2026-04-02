#' Get the sensitivity of total biomass to temperature variation. Currently measured as the slope of a linear regression of total biomass on temperature.
#'
#' @param dynamics Connection to database containing dynamics data
#' @param temperatures Connection to database containing temperature data
#' @param expt Dataset containing experiment information
#' @param rollsumr_window The window size for the rolling sum of temperature. Default is 50
#'
#' @return A dataset containing the sensitivity of total biomass to temperature variation
#' @export
#'
#' @examples NULL
get_community_temp_sens <- function(dynamics,
                                    temperatures,
                                    rollsumr_window = 50,
                                    expt) {

  sub_expt <- expt |>
    dplyr::select(case_id, env_series_id, community_id)

  ## calculate temperature sensitivity of total biomass
  ## get rolling sum of temperatures
  temperatures <- temperatures |>
    dplyr::collect()

  temperatures <- temperatures |>
    dplyr::group_by(env_series_id) |>
    dplyr::mutate(temperature_rollsum = zoo::rollsumr(temperature, rollsumr_window, fill = NA))

  ## calculate total biomass
  temp0 <- dynamics |>
    dplyr::group_by(case_id, time) |>
    dplyr::summarise(tot_ab = sum(Abundance, na.rm = FALSE)) |>
    dplyr::collect()

  temp1 <- temp0 |>
    dplyr::full_join(sub_expt, by = c("case_id" = "case_id"))

  ## calculate the temperature sensitivity of total biomass
  ## merge the temperature and biomass time series
  dd <- dplyr::full_join(temp1, temperatures, by = c("env_series_id" = "env_series_id", "time" = "time")) |>
    dplyr::select(case_id, time, temperature, temperature_rollsum, tot_ab)

  ## make a dataset without any NA or Inf
  ## cases with any Infinite values in tot_ab
  cases_with_inf_or_NA <- dd |>
    dplyr::filter(is.infinite(tot_ab) | is.na(tot_ab)) |>
    dplyr::pull(case_id) |>
    unique()
  dd_OK <- dd |>
    dplyr::filter(!(case_id %in% cases_with_inf_or_NA))


  temp_sens <- dd_OK |>
    tidyr::nest(data = c(time, temperature, temperature_rollsum, tot_ab)) |>
    dplyr::mutate(model = purrr::map(data, ~ stats::lm(tot_ab ~ temperature, data = .))) |>
    dplyr::mutate(tidy_model = purrr::map(model, broom::tidy)) |>
    tidyr::unnest(tidy_model) |>
    dplyr::filter(term == "temperature")

  temp_sens_to_merge_rs <- temp_sens |>
    dplyr::select(case_id, comm_temperature_sensitivity = estimate)

  temp_sens_to_merge_rs <- dplyr::bind_rows(
    temp_sens_to_merge_rs,
    data.frame(
      case_id = cases_with_inf_or_NA,
      comm_temperature_sensitivity = rep(NA, length(cases_with_inf_or_NA))
    )
  )


  return(temp_sens_to_merge_rs)
}
