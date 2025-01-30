#' Create temperature times series. Currently three options for how times series vary among cases.
#' 1) `same_per_replicate` replicates of the same number share the same environmental time series. E.g., `case1_rep1` and `case2_rep1` share the same time series.
#' 2) `all_different` all environmental time series are different
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series.
#' @export
#'
#' @examples NULL
create_environments <- function(experiment_folder,
                                experiment_design_filename) {

  ## setup the databases for saving the temperature time series
  file.remove(paste0(experiment_folder, "temperatures.db"))
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))

  expt <- readRDS(paste0(experiment_folder, "experiment_table.rds"))

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  expt_def$random_seed

  i <- 1
  for(i in 1:nrow(expt)){

    print(i)


    ## keep the next line to have a different seed for each replicate
    if(expt$temperature_series_control[i] == "same_per_replicate")
      set.seed(expt_def$random_seed + abs(parse_number(as.character(expt$rep_names[i]))))
    if(expt$temperature_series_control[i] == "all_different")
      set.seed(expt_def$random_seed + i)


    temperature_series <- tibble(phase = c(rep("burn_in", expt_def$burn_in_duration),
                                           rep("expt", expt_def$experiment_duration + 1)),

                                 time = 0:(expt_def$burn_in_duration +
                                             expt_def$experiment_duration),

                                 temperature = c(
                                   ## first the burn in phase, with temperature constant at the mean
                                   rep(expt_def$temperature_mean,
                                       expt_def$burn_in_duration),
                                   ## then the experiment phase, with temperature fluctuating
                                   scale(
                                     one_over_f(gamma = expt$one_over_f_gamma[i],
                                                N = expt_def$experiment_duration+1)) *
                                     expt_def$temperature_sd + expt_def$temperature_mean),

                                 case_id = expt$case_id[i])

    # Tcel_control<-temperature_series$temperature
    # Tcel_controlm<-matrix(Tcel_control,nrow=1)
    #
    #
    #
    #
    # S <- expt[i,]$community_object[[1]]$S
    #
    # initial_abundances <- (rdirichlet(1, rep(1, S))*1000)[1,]
    #
    #
    #
    # spts <- simulator_lv(input_com_params = expt$community_object[[i]],
    #                      TcelSeries = Tcel_controlm,
    #                      initial_abundances = initial_abundances)
    #
    #
    #
    # spts <- spts |>
    #   as_tibble() |>
    #   mutate(case_id = expt$case_id[i],
    #          time = temperature_series$time) |>
    #   pivot_longer(names_to = "Species_ID", values_to = "Abundance",
    #                cols = starts_with("Spp")) |>
    #   filter(time > expt_def$burn_in_duration)
    #

    # ggplot(spts, aes(x = time, y = Abundance, color = Species_ID)) +
    #    geom_line() +
    #   labs(title = paste("Case ID:", expt$case_id[i]))

    temperature_series_expt_only <- temperature_series |>
      filter(time > expt_def$burn_in_duration)

    if(i == 1) {
      dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, append = TRUE)
    }


  }

  dbDisconnect(conn_temperatures)

}
