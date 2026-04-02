#' Simulate the dynamics of all the cases in an experiment
#' Unfortunately at the moment has features of temperature series hard coded in
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing dynamics database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series and the community dynamics.
#' @export
#'
#' @examples NULL
simulate_dynamics <- function(experiment_folder,
                              experiment_design_filename,
                              overwrite = FALSE,
                              verbose = TRUE) {

  ## setup the databases for saving the temperature time series and the community dynamics
  ## set up data base to save results into
  output_path <- paste0(experiment_folder, "dynamics.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "dynamics database")
  conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), output_path)
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))


  i <- 4
  for(i in 1:nrow(expt)){

    if (verbose) {
      message("Simulating case ", i, " of ", nrow(expt))
    }

    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      dplyr::filter(env_series_id == env_series_oi) |>
      dplyr::collect()

    burn_in_temps <- tibble::tibble(phase = rep("burn_in", expt_def$burn_in_duration),
                                    time = 1:expt_def$burn_in_duration,
                                    temperature = rep(expt_def$temperature_mean,
                                                      expt_def$burn_in_duration),
                                    env_series_id = rep(env_series_oi, expt_def$burn_in_duration)
                            #case_id = rep(case_id_oi, expt_def$burn_in_duration)
                            )
    temperature_series <- dplyr::bind_rows(burn_in_temps, temperatures_oi)


    Tcel_control<-temperature_series$temperature
    Tcel_controlm<-matrix(Tcel_control,nrow=1)




    S <- expt[i,]$community_object[[1]]$S

    initial_abundances <- (dirmult::rdirichlet(1, rep(1, S)) * 1000)[1,]



    spts <- simulator_lv(input_com_params = expt$community_object[[i]],
                         TcelSeries = Tcel_controlm,
                         initial_abundances = initial_abundances)



    spts <- spts |>
      tibble::as_tibble() |>
      dplyr::mutate(case_id = expt$case_id[i],
                    time = temperature_series$time) |>
      tidyr::pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                          cols = dplyr::starts_with("Spp")) |>
      dplyr::filter(time > expt_def$burn_in_duration)


   # ggplot(spts, aes(x = time, y = Abundance, color = Species_ID)) +
  #    geom_line() +
   #   labs(title = paste("Case ID:", expt$case_id[i]))

    #temperature_series_expt_only <- temperature_series |>
    #  filter(time > expt_def$burn_in_duration)

    if(i == 1) {
      DBI::dbWriteTable(conn_dynamics, "dynamics", spts, overwrite = TRUE)
    }
    if(i > 1) {
      DBI::dbWriteTable(conn_dynamics, "dynamics", spts, append = TRUE)
    }


    }

  DBI::dbDisconnect(conn_dynamics)
  announce_output_written(output_path, verbose = verbose, label = "dynamics database")

}
