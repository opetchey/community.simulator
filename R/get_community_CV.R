#' Get the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#'
#' @param dynamics Connection to database containing dynamics data
#' @param expt Dataset containing experiment information
#'
#' @return A dataset containing the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#' @export
#'
#' @examples NULL
get_community_CV <- function(dynamics) {

  ## Community biomass and community stability ----
  temp1 <- dynamics |>
    group_by(case_id, time) %>%
    summarise(tot_ab = sum(Abundance, na.rm = F)) |>
    collect()
  comm_stab <- temp1 %>%
    group_by(case_id) %>%
    summarise(mean_totab = mean(tot_ab),
              sd_totab = sd(tot_ab),
              CV_totab = sd_totab / mean_totab)
    #full_join(expt) |>
    #select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)

  return(comm_stab)
}

