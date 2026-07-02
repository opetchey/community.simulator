get_CV_com_perf <- function(experiment_folder,
                          experiment_design_filename,
                          every_t = 1) {

  # Open connections and read in data
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  # Remove previous database if it exists
  file.remove(paste0(experiment_folder, "imbalance.db"))
  conn_imbalance <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "imbalance.db"))

  # Initialize list to collect results
  all_CV_community_perf <- list()

  # Loop through each case
  for (i in seq_along(expt$case_id)) {

    if(i %% 100 == 0){
      message("Processing case: ", i)
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



    species_pars2<-lapply(splt_spec,function(df){
      temp_range<-seq(min(species_pars1$temperatures),max(species_pars1$temperatures),length.out=length(df$temperatures))
      curve <- df %>%
        dplyr::mutate(igr = intrinsic_growth_gaussian(
          a_b_i, b_opt_i, s_i, a_d_i, z_i, temp_range
        ),
        temp=temp_range)
    })

    species_pars2<-do.call(rbind,species_pars2)

  # Calculate Loreau synchrony

    synchrony <- species_pars2 %>%
      group_by(case_id) %>%
      group_modify(~ {
        df <- .x


        value <- synchrony(df, time = "temp", species = "species_id",
                           abundance = "igr", metric = "Loreau")
        tibble(synchrony_perf = value)
      })

    # Get average population stability

    pop_stab<-species_pars2 %>%
      group_by(case_id,species_id) %>%
      group_modify(~ {
        df <- .x
        value = CV=sd(df$igr)/mean(df$igr)
        tibble(pop_CV = value)
      }) %>% group_by (case_id) %>%
      summarise(mean_pop_CV_perf=mean(pop_CV))

    species_pars3<-species_pars2%>%
      dplyr::group_by(case_id,temp) %>%
      dplyr::summarise(community_igr=sum(igr),.groups = "drop")



    species_pars4<-species_pars3%>%
      dplyr::group_by(case_id) %>%
      dplyr::summarise(CV_community_perf=sd(community_igr)/mean(community_igr))


    species_pars4<-species_pars4 %>% full_join(synchrony) %>%suppressMessages()
    species_pars4<-species_pars4 %>% full_join(pop_stab) %>%suppressMessages()

    all_CV_community_perf[[i]] <- species_pars4



    # if (i == 1) {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, overwrite = TRUE)
    # } else {
    #   DBI::dbWriteTable(conn_imbalance, "imbalance", imbalance, append = TRUE)
    # }
  }

  DBI::dbDisconnect(conn_imbalance)
  DBI::dbDisconnect(conn_temperatures)

  # Combine and return
  final_CV <- dplyr::bind_rows(all_CV_community_perf)
  return(final_CV)
}


