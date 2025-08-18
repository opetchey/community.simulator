#' Get the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#'
#' @param dynamics Connection to database containing dynamics data
#' @param expt Dataset containing experiment information
#'
#' @return A dataset containing the stability of each case (community). Calculated CV of total biomass temporal variation. Also returns mean total biomass and standard deviation of total biomass
#' @export
#'
#' @examples NULL
get_community_popstab <- function(dynamics) {





  ## Sd on pop_level
  temp_pop <- dynamics |>
    group_by(case_id,Species_ID ) %>%
    summarise(sd_pop = sd(Abundance),
              mean_ab_pop=mean(Abundance)) |>
    collect()

  temp_pop <- temp_pop |>
    mutate(CV_ab_pop=sd_pop/mean_ab_pop)

  temp1 <- dynamics |>
    group_by(case_id, time) %>%
    summarise(tot_ab = sum(Abundance, na.rm = F)) |>
    collect()



  comm_stab <- temp1 %>%
    group_by(case_id) %>%
    summarise(mean_totab=mean(tot_ab))

  ## Jget CV_pop_ab

  temp_2 <- full_join(comm_stab, temp_pop, by = "case_id")
  temp_2<-temp_2%>%
    mutate(rel_ab=mean_ab_pop/mean_totab,
           CV_ab_pop=sd_pop/mean_ab_pop) |>
    group_by(case_id) %>%
    summarise(pop_CV_ab=sum(rel_ab*CV_ab_pop) )






  #full_join(expt) |>
  #select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)

  return(temp_2)
}

