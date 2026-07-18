#' Get realized performance-optimum summaries relative to environmental temperature
#'
#' @param temperatures Connection to the database containing temperature data
#' @param expt Dataset containing experiment information
#'
#' @return A dataset containing realized performance-optimum summaries relative
#'   to the mean environmental temperature experienced by each community.
#' @export
#'
#' @examples NULL
get_community_performance_optimum_measures <- function(temperatures, expt) {

  mean_temps <- temperatures |>
    dplyr::group_by(env_series_id) |>
    dplyr::summarise(mean_temperature = mean(temperature, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()

  expt_long <- tidyr::unnest_longer(expt, col = c(community_object)) |>
    dplyr::filter(community_object_id %in% c("birth_optimum_i", "uptake_optimum_i")) |>
    tidyr::unnest(cols = c(community_object)) |>
    dplyr::mutate(
      performance_optimum_trait = dplyr::case_when(
        community_object_id == "birth_optimum_i" ~ "birth_optimum",
        community_object_id == "uptake_optimum_i" ~ "uptake_optimum",
        TRUE ~ community_object_id
      )
    ) |>
    dplyr::group_by(case_id) |>
    dplyr::mutate(species_id = rep(paste0("Spp-", seq_along(case_id))))

  relative_performance_optimum <- dplyr::full_join(mean_temps, expt_long, by = "env_series_id") |>
    dplyr::mutate(relative_performance_optimum = community_object - mean_temperature)

  community_relative_performance_optimum <- relative_performance_optimum |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(
      performance_optimum_trait = dplyr::first(performance_optimum_trait),
      sum_relative_performance_optimum = sum(relative_performance_optimum),
      minimum_absolute_relative_performance_optimum = min(abs(relative_performance_optimum)),
      .groups = "drop"
    )

  realized_performance_optimum <- expt_long |>
    dplyr::group_by(community_id, case_id) |>
    dplyr::summarise(
      performance_optimum_trait = dplyr::first(performance_optimum_trait),
      realized_mean_performance_optimum = mean(community_object),
      realized_sd_performance_optimum = sqrt(
        sum((community_object - mean(community_object))^2) /
          length(community_object)
      ),
      .groups = "drop"
    )

  result <- community_relative_performance_optimum |>
    dplyr::left_join(
      realized_performance_optimum,
      by = c("case_id", "performance_optimum_trait")
    )


  return(result)
}

# Legacy helper retained for old internal code paths.
get_community_sum_rel_b_opt <- function(temperatures, expt) {
  get_community_performance_optimum_measures(temperatures, expt)
}
