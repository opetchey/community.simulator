#' Get measures of community performance curves
#'
#' @param temperatures - temperature time series used in the simulations
#' @param expt - experiment table with community parameters
#' @param expt_def - experiment design information
#' @param every_t - how often to sample the temperature time series (e.g., every 1 time step, every 10 time steps, etc.)
#'
#' @returns A dataset containing measures of community performance curves, including CV of community performance, synchrony of performance curves, and average CV of species performance curves weighted by their mean contribution to community performance.
#' @export
#'
#' @examples NULL
get_community_CPC_measures <- function(temperatures,
                                 expt,
                                 expt_def,
                                 every_t = 1) {



  # Open connections and read in data
  #conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  #temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  #expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  #expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  # Remove previous database if it exists
  #file.remove(paste0(experiment_folder, "imbalance.db"))
  #conn_imbalance <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "imbalance.db"))



  # Initialize list to collect results
  all_CV_community_perf <- list()

  # Loop through each case
  for (i in seq_along(expt$case_id)) {

    if(i %% 100 == 0){
      print(paste0("Processing CPC case: ", i))
    }


    case_id_oi <- expt$case_id[i]
    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      dplyr::filter(env_series_id == env_series_oi) |>
      dplyr::collect() |>
      dplyr::filter((time %% every_t) == 0)

    # Extract community parameters
    comm_pars_i <- expt$community_object[[i]]
    species_pars <- tibble::tibble(
      case_id = rep(case_id_oi, length(comm_pars_i$b_opt_i)),
      species_id = paste0("Spp-", seq_along(comm_pars_i$b_opt_i)),
      b_opt_i = comm_pars_i$b_opt_i,
      a_b_i = comm_pars_i$a_b_i,
      s_i = comm_pars_i$s_i,
      a_d_i = comm_pars_i$a_d_i,
      z_i = comm_pars_i$z_i
    )

    species_pars1 <- species_pars %>%
      dplyr::mutate(
        temperatures = list(temperatures_oi$temperature),
        time = list(temperatures_oi$time)
      ) %>%
      tidyr::unnest(cols = c(temperatures, time))

    splt_spec<-species_pars1%>%split(species_pars1$species_id)


    type=c("info","naive")



    species_pars2_naive<-lapply(splt_spec,function(df){
      temp_range<-seq(min(species_pars1$temperatures),max(species_pars1$temperatures),length.out=length(df$temperatures))
      curve <- df %>%
        dplyr::mutate(igr_naive = intrinsic_growth_gaussian(
          a_b_i, b_opt_i, s_i, a_d_i, z_i, temp_range
        ),
        temp=temp_range)
    })

    species_pars2_naive<-do.call(rbind,species_pars2_naive)

    species_pars2_info<-lapply(splt_spec,function(df){
      temp_range<-temperatures_oi$temperature
      curve <- df %>%
        dplyr::mutate(igr_info = intrinsic_growth_gaussian(
          a_b_i, b_opt_i, s_i, a_d_i, z_i, temp_range
        ),
        temp=temp_range)
    })

    species_pars2_info<-do.call(rbind,species_pars2_info)%>%select(c("case_id","species_id","igr_info","time"))

    species_pars2<-full_join(species_pars2_naive,species_pars2_info)%>%suppressMessages()


    tot_performance<-species_pars2%>%
      dplyr::group_by(case_id,temp)%>%
      dplyr::summarise(community_igr_info=sum(igr_info),
                       community_igr_naive=sum(igr_naive),.groups = "drop")

    # Calculate Loreau synchrony of performance curves

    tot_var<-tot_performance%>%
      group_by(case_id)%>%
      summarise(tot_var_info= sd(community_igr_info)^2,
                tot_var_naive= sd(community_igr_naive)^2,.groups = "drop")



    species_sd <- species_pars2 %>%
      group_by(case_id,species_id) %>%
      summarise(spec_sd_info=sd(igr_info),
                spec_sd_naive=sd(igr_naive))%>%
      group_by(case_id)%>%
      summarise(sum_sd_2_info=(sum(spec_sd_info))^2,
                sum_sd_2_naive=(sum(spec_sd_naive))^2)

    synchrony<-full_join(species_sd,tot_var,by="case_id")%>%suppressMessages()%>%
      group_by(case_id)%>%
      summarise(synchrony_perf_info=tot_var_info/ sum_sd_2_info,
                synchrony_perf_naive=tot_var_naive/ sum_sd_2_naive,.groups = "drop" )


    # Get average CV

    tot_mean<-tot_performance%>%
      group_by(case_id)%>%
      summarise(tot_mean_info= mean(community_igr_info),
                tot_mean_naive= mean(community_igr_naive),.groups = "drop")

    spec_CV<-species_pars2 %>%
      group_by(case_id,species_id) %>%
      summarise(mean_igr_info=mean(igr_info),
                mean_igr_naive=mean(igr_naive),
                sd_igr_info=sd(igr_info),
                sd_igr_naive=sd(igr_naive))%>%
      mutate(spec_CV_info=sd_igr_info/mean_igr_info,
             spec_CV_naive=sd_igr_naive/mean_igr_naive)

    avg_spc_CV<-full_join(spec_CV,tot_mean,by="case_id")%>%suppressMessages()%>%
      group_by(case_id)%>%
      summarise(avg_perf_CV_info=sum( (mean_igr_info / tot_mean_info) * spec_CV_info),
                avg_perf_CV_naive=sum( (mean_igr_naive / tot_mean_naive) * spec_CV_naive),.groups = "drop")







    species_pars4<-tot_performance%>%
      dplyr::group_by(case_id) %>%
      dplyr::summarise(CV_community_perf_info=sd(community_igr_info)/mean(community_igr_info),
                       CV_community_perf_naive=sd(community_igr_naive)/mean(community_igr_naive),.groups = "drop")


    species_pars4<-species_pars4 %>% full_join(synchrony) %>%suppressMessages()
    species_pars4<-species_pars4 %>% full_join(avg_spc_CV) %>%suppressMessages()

    all_CV_community_perf[[i]] <- species_pars4



    # if (i == 1) {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, overwrite = TRUE)
    # } else {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, append = TRUE)
    # }
  }

  #DBI::dbDisconnect(conn_imbalance)
  #DBI::dbDisconnect(conn_temperatures)

  # Combine and return
  final_CV <- dplyr::bind_rows(all_CV_community_perf)
  return(final_CV)
}


