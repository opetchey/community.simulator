#' Get the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#'
#' @param dynamics Connection to database containing dynamics data
#'
#' @return A dataset containing the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#' @export
#'
#' @examples NULL
get_community_syn<- function(dynamics) {


  ## Sd on pop_level
  temp_pop <- dynamics |>
    dplyr::group_by(case_id, Species_ID) |>
    dplyr::summarise(sd_pop = stats::sd(Abundance)) |>
    dplyr::collect()

  temp_pop_2 <- temp_pop |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(sum_sd = sum(sd_pop))


  ## Community biomass and community stability ----
  temp1 <- dynamics |>
    dplyr::group_by(case_id, time) |>
    dplyr::summarise(tot_ab = sum(Abundance, na.rm = FALSE)) |>
    dplyr::collect()

  ## Join pop level and commuinity level



  comm_stab <- temp1 |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(sd_totab = stats::sd(tot_ab))

  ## Join pop level and commuinity level

  temp_2 <- dplyr::full_join(comm_stab, temp_pop_2, by = "case_id")
  temp_2 <- temp_2 |>
    dplyr::group_by(case_id) |>
    dplyr::summarise(sync_ab = sd_totab^2 / sum_sd^2)

  #full_join(expt) |>
  #select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)

  return(temp_2)
}
