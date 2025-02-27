#' Get derivatives at arbitrary temperatures
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The file with the experiment design
#'
#' @return Data base with the derivatives at arbitrary temperatures
#' @export
#'
#' @examples NULL
get_arbitrary_derivatives <- function(experiment_folder,
                                     experiment_design_filename) {

  ## open connections and read in data
  #conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- arrow::open_dataset(paste0(experiment_folder, "temperatures"))
  expt <- readRDS(paste0(experiment_folder, "experiment_table.rds"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  #file.remove(paste0(experiment_folder, "arbitrary_derivs.db"))
  #conn_derivs <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "arbitrary_derivs.db"))


  ## For each case
  i <- 1
  for(i in 1:length(expt$case_id)) {

    print(i)

    ## Get a sequences of temperatures, using mean and standard deviation
    ## from the expt table
    min_temperature <- expt$temperature_mean[i] - 2.5 * expt$temperature_sd[i]
    max_temperature <- expt$temperature_mean[i] + 2.5 * expt$temperature_sd[i]

    temperatures_oi <- tibble(temperature = seq(min_temperature,
                                                max_temperature,
                                                length = 100))

    ## Get the parameters for each species in the community
    comm_pars_i <- expt$community_object[i][[1]]
    species_pars <- tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                           species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                           b_opt_i = comm_pars_i$b_opt_i,
                           a_b_i = comm_pars_i$a_b_i,
                           s_i = comm_pars_i$s_i,
                           a_d_i = comm_pars_i$a_d_i,
                           z_i = comm_pars_i$z_i)

    ## Combine the parameters with the temperatures
    species_pars1 <- species_pars %>%
      mutate(temperatures = list(temperatures_oi$temperature)) %>%
      unnest(cols = temperatures)

    ## Calculate the intrinsic growth rate for each species at each temperature
    species_pars2 <- species_pars1 %>%
      mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperatures))

    ## Fit a GAM to the intrinsic growth rate - temperature relationship
    species_pars3 <- species_pars2 %>%
      nest_by(case_id, species_id) %>%
      mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 10),
                                     data = data))) %>%
      select(-data)

    ## Get the derivatives of the GAM at the arbitrary temperatures
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


    arrow::write_dataset(species_pars4,
                         path = paste0(experiment_folder, "arbitrary_derivatives"),
                         format = "parquet",
                         partitioning = "case_id",
                         existing_data_behavior = "overwrite")


    # if(i == 1) {
    #   dbWriteTable(conn_derivs, "derivs", species_pars4, overwrite = TRUE)
    # }
    # if(i > 1) {
    #   dbWriteTable(conn_derivs, "derivs", species_pars4, append = TRUE)
    # }

  }

  #dbDisconnect(conn_derivs)
  #dbDisconnect(conn_temperatures)



}
