simulate_dynamics <- function(data_location_for_experiment,
                              expt_definition_filename) {

  ## setup the databases for saving the temperature time series and the community dynamics
  ## set up data base to save results into
  #library(RSQLite)
  #library(DBI)
  file.remove(paste0(data_location_for_experiment, "dynamics.db"))
  file.remove(paste0(data_location_for_experiment, "temperatures.db"))
  conn_dynamics <- dbConnect(RSQLite::SQLite(), paste0(data_location_for_experiment, "dynamics.db"))
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(data_location_for_experiment, "temperatures.db"))

  expt <- readRDS(paste0(data_location_for_experiment, "experiment_table.rds"))


  expt_def <- read.csv(paste0(data_location_for_experiment, expt_definition_filename))

  lookup_object_value <- function(expt_def, this_object) {
    #check if object can be parsed as numeric
    result <- suppressWarnings(as.numeric(expt_def$Value[expt_def$Object == this_object]))
    if(is.na(result))
      result <- as.character(expt_def$Value[expt_def$Object == this_object])
    result
  }



  i <- 1
  for(i in 1:nrow(expt)){

    print(i)


    ## keep the next line to have a different seed for each replicate
    if(expt$temperature_series_control[i] == "all_same")
      set.seed(seed.to.use)
    if(expt$temperature_series_control[i] == "all_different")
      set.seed(seed.to.use + abs(parse_number(as.character(expt$rep_names[i]))))

    temperature_series <- tibble(phase = c(rep("burn_in", lookup_object_value(expt_def, "burn_in_duration")),
                                           rep("expt", lookup_object_value(expt_def, "experiment_duration")+1)),
                                 time = 0:(lookup_object_value(expt_def, "burn_in_duration") +
                                             lookup_object_value(expt_def, "experiment_duration")),
                                 temperature = c(
                                   ## first the burn in phase, with temperature constant at the mean
                                   rep(lookup_object_value(expt_def, "temperature_mean"),
                                       lookup_object_value(expt_def, "burn_in_duration")),
                                   ## then the experiment phase, with temperature fluctuating
                                   scale(
                                     one_over_f(gamma = 0.8, N = lookup_object_value(expt_def, "experiment_duration")+1)
                                   ) *
                                     lookup_object_value(expt_def, "temperature_sd") + lookup_object_value(expt_def, "temperature_mean")),
                                 case_id = expt$case_id[i])
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
      filter(time > lookup_object_value(expt_def, "burn_in_duration"))


   # ggplot(spts, aes(x = time, y = Abundance, color = Species_ID)) +
  #    geom_line() +
   #   labs(title = paste("Case ID:", expt$case_id[i]))

    temperature_series_expt_only <- temperature_series |>
      filter(time > lookup_object_value(expt_def, "burn_in_duration"))

    if(i == 1) {
      dbWriteTable(conn_dynamics, "dynamics", spts, overwrite = TRUE)
      dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_dynamics, "dynamics", spts, append = TRUE)
      dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, append = TRUE)
    }
  }




}
