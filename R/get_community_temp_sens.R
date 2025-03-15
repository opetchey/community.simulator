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
    select(case_id, env_series_id, community_id)

  ## calculate temperature sensitivity of total biomass
  ## get rolling sum of temperatures
  temperatures <- temperatures |>
    collect()

  temperatures <- temperatures |>
    group_by(env_series_id) |>
    mutate(temperature_rollsum = zoo::rollsumr(temperature, rollsumr_window, fill = NA))

  ## calculate total biomass
  temp0 <- dynamics |>
    group_by(case_id, time) %>%
    summarise(tot_ab = sum(Abundance, na.rm = F)) |>
    collect()

  temp1 <- temp0 |>
    full_join(sub_expt, by = c("case_id" = "case_id"))

  ## calculate the temperature sensitivity of total biomass
  ## merge the temperature and biomass time series
  dd <- full_join(temp1, temperatures, by = c("env_series_id" = "env_series_id", "time" = "time")) |>
    select(case_id, time, temperature, temperature_rollsum, tot_ab)

  ## make a dataset without any NA or Inf
  ## cases with any Infinite values in tot_ab
  cases_with_inf_or_NA <- dd |>
    filter(is.infinite(tot_ab) | is.na(tot_ab)) |>
    pull(case_id) |>
    unique()
  dd_OK <- dd |>
    filter(!(case_id %in% cases_with_inf_or_NA))


  temp_sens <- dd_OK |>
    nest(data = c(time, temperature, temperature_rollsum, tot_ab)) |>
    mutate(model = map(data, ~ lm(tot_ab ~ temperature, data = .))) |>
    mutate(tidy_model = map(model, tidy)) %>%
    unnest(tidy_model) |>
    filter(term == "temperature")

  temp_sens_to_merge_rs <- temp_sens %>%
    select(case_id, comm_temperature_sensitivity = estimate)

  temp_sens_to_merge_rs <- bind_rows(temp_sens_to_merge_rs,
                                     data.frame(case_id = cases_with_inf_or_NA,
                                                comm_temperature_sensitivity = rep(NA, length(cases_with_inf_or_NA))))


  return(temp_sens_to_merge_rs)
}
