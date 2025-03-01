#' Simulate the dynamics of all the cases in an experiment
#' Unfortunately at the moment has features of temperature series hard coded in
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series and the community dynamics.
#' @export
#'
#' @examples NULL
simulate_dynamics <- function(experiment_folder,
                              experiment_design_filename) {

  ## setup the databases for saving the temperature time series and the community dynamics
  ## set up data base to save results into
  #library(RSQLite)
  #library(DBI)
  file.remove(paste0(experiment_folder, "dynamics.db"))
  conn_dynamics <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "dynamics.db"))
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))


  i <- 7
  for(i in 1:nrow(expt)){

    print(i)

    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      filter(env_series_id == env_series_oi) |>
      collect()

    burn_in_temps <- tibble(phase = rep("burn_in", expt_def$burn_in_duration),
                            time = 1:expt_def$burn_in_duration,
                            temperature = rep(expt_def$temperature_mean,
                                expt_def$burn_in_duration),
                            env_series_id = rep(env_series_oi, expt_def$burn_in_duration)
                            #case_id = rep(case_id_oi, expt_def$burn_in_duration)
                            )
    temperature_series <- bind_rows(burn_in_temps, temperatures_oi)


    Tcel_control<-temperature_series$temperature
    Tcel_controlm<-matrix(Tcel_control,nrow=1)




    S <- expt[i,]$community_object[[1]]$S

    initial_abundances <- (rdirichlet(1, rep(1, S))*1000)[1,]



    spts <- simulator_lv(input_com_params = expt$community_object[[i]],
                         TcelSeries = Tcel_controlm,
                         initial_abundances = initial_abundances)



    spts <- spts |>
      as_tibble() |>
      mutate(case_id = expt$case_id[i],
             time = temperature_series$time) |>
      pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                   cols = starts_with("Spp")) |>
      filter(time > expt_def$burn_in_duration)


   # ggplot(spts, aes(x = time, y = Abundance, color = Species_ID)) +
  #    geom_line() +
   #   labs(title = paste("Case ID:", expt$case_id[i]))

    #temperature_series_expt_only <- temperature_series |>
    #  filter(time > expt_def$burn_in_duration)

    if(i == 1) {
      dbWriteTable(conn_dynamics, "dynamics", spts, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_dynamics, "dynamics", spts, append = TRUE)
    }


    }

  dbDisconnect(conn_dynamics)

}
