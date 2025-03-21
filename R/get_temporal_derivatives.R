#' Calculate the derivatives of the growth rate - temperature relationship at the temperatures in the temperature times series of each case in the experiment.
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The name of the experiment design file
#' @param every_t The time interval at which to calculate the derivatives
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_temporal_derivatives <- function(experiment_folder,
                                     experiment_design_filename,
                                     every_t = 10) {

  ## open connections and read in data
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  file.remove(paste0(experiment_folder, "temporal_derivs.db"))
  conn_derivs <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temporal_derivs.db"))


  ## Expand expt to make a species in row dataset
  i = 1
  for(i in 1:length(expt$case_id)) {

    ## Housekeeping
    print(i)
    case_id_oi <- expt$case_id[i]
    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      filter(env_series_id == env_series_oi) |>
      collect() |>
      filter((time %% every_t) == 0)

    ## Get the community parameters for this case
    comm_pars_i <- expt$community_object[i][[1]]
    species_pars <- tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                           species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                           b_opt_i = comm_pars_i$b_opt_i,
                           a_b_i = comm_pars_i$a_b_i,
                           s_i = comm_pars_i$s_i,
                           a_d_i = comm_pars_i$a_d_i,
                           z_i = comm_pars_i$z_i)

    ## Combine species parameters with temperatures
    species_pars1 <- species_pars %>%
      mutate(temperatures = list(temperatures_oi$temperature)) %>%
      unnest(cols = temperatures)

    ## Calculate the intrinsic growth rate for each species
    ## at each temperature in the time series
    species_pars2 <- species_pars1 %>%
      mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperatures))

    ## Fit a gam to the growth rate - temperature relationship
    species_pars3 <- species_pars2 %>%
      nest_by(case_id, species_id) %>%
      mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 10),
                                     data = data))) %>%
      select(-data)

    ## make a ggplot of one of the species igr temperature relationships
    #species_pars2 |>
    #  filter(species_id == "Spp-2") |>
    #  ggplot(aes(x = temperatures, y = igr)) +
    #  geom_point()

    ## Calculate the derivative of the gam of the growth rate - temperature relationship
    species_pars4 <- full_join(species_pars1, species_pars3) %>%
      group_by(case_id, species_id) %>%
      mutate(new_data = list(data.frame(temperatures = temperatures))) %>%
      select(-temperatures) %>%
      unique() %>%
      rowwise() %>%
      mutate(derivative = list(gratia::derivatives(models,
                                                   data = new_data))) %>%
      unnest(derivative) %>%
      select(-models, -new_data) %>%
      rename(temperature = temperatures, derivative = .derivative) %>%
      mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperature)) %>%
      select(case_id, species_id, temperature, igr, derivative)


    ## make a ggplot of one of the species igr temperature relationships
    #species_pars4 |>
    #  filter(species_id == "Spp-1") |>
    #  ggplot(aes(x = temperature, y = igr)) +
    #  geom_line(col= "red") +
    #  geom_line(aes(y = derivative), col = "blue")


  ## Write to database
    if(i == 1) {
      dbWriteTable(conn_derivs, "derivs", species_pars4, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_derivs, "derivs", species_pars4, append = TRUE)
    }

  }

  dbDisconnect(conn_derivs)
  dbDisconnect(conn_temperatures)

}
