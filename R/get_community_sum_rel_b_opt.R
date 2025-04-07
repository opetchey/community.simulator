#' Get the sum of the relative position of the temperature optimum of each species. Relative to the mean of the environmental temperatures experienced by the community.
#'
#' @param temperatures Connection to the database containing temperature data
#' @param expt Dataset containing experiment information
#'
#' @return A dataset containing the sum of the relative position of the temperature optimum of each species. Relative to the mean of the environmental temperatures experienced by the community.
#' @export
#'
#' @examples NULL
get_community_sum_rel_b_opt <- function(temperatures, expt) {

  ## sum of relative position of temperature optima
  ## relative to mean environmental temperature each simulation
  mean_temps <- temperatures |>
    group_by(env_series_id) %>%
    summarise(mean_temperature = mean(temperature)) |>
    collect()
  expt_long <- unnest_longer(expt, col = c(community_object)) |>
    filter(community_object_id == "b_opt_i") |>
    unnest(cols = c(community_object)) |>
    group_by(case_id) |>
    mutate(species_id = rep(paste0("Spp-", 1:length(case_id))))
  rel_b_opt <- full_join(mean_temps, expt_long) |>
    mutate(relative_b_opt = community_object - mean_temperature)
  comm_sum_rel_b_opt <- rel_b_opt |>
    group_by(case_id) |>
    summarise(sum_rel_b_opt = sum(relative_b_opt),
              min_rel_b_opt = min(abs(relative_b_opt)))


  expt_long <- unnest_longer(expt, col = c(community_object)) |>
    filter(community_object_id == "b_opt_i") |>
    unnest(cols = c(community_object)) |>
    group_by(case_id) |>
    mutate(species_id = rep(paste0("Spp-", 1:length(case_id))))
  real_b_opt <- expt_long |>
    group_by(community_id, case_id) |>
    summarise(real_mean_b_opt = mean(community_object),
              real_sd_b_opt = sqrt( sum( (community_object-mean(community_object))^2 ) /
                                      length(community_object)) )

  result <- comm_sum_rel_b_opt |>
    left_join(real_b_opt, by = "case_id")


  return(result)
}
