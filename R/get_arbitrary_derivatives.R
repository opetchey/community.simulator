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
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.rds"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  file.remove(paste0(experiment_folder, "arbitrary_derivs.db"))
  conn_derivs <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "arbitrary_derivs.db"))


  ## Get a sequences of temperatures, using the min and max of the first
  ## case_id and a buffer of 5 above and below
  ## and length of 200
  i = 1
  #print(i)
  case_id_oi <- expt$case_id[i]
  temp1 <- temperatures |>
    filter(case_id == case_id_oi) |>
    collect() |>
    summarise(min = min(temperature),
              max = max(temperature))
  buffer <- 5
  temperatures_oi <- tibble(temperature = seq(temp1$min-buffer,
                                              temp1$max+buffer,
                                              length = 200))



  ## For each case
  for(i in 1:length(expt$case_id)) {

    print(i)
    # case_id_oi <- expt$case_id[i]
    # temperatures_oi <- temperatures |>
    #   filter(case_id == case_id_oi) |>
    #   collect() |>
    #   filter((time %% 10) == 0)
    #
    #

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
