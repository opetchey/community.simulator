#' Calculate the difference in growth rate from one time point to the next
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The name of the experiment design file
#' @param every_t The time interval at which to calculate the difference
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_delta_igr <- function(experiment_folder,
                          experiment_design_filename,
                          every_t = 1) {

  ## open connections and read in data
  conn_temperatures <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  file.remove(paste0(experiment_folder, "delta_igr.db"))
  conn_delta_igr <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "delta_igr.db"))


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
      mutate(temperatures = list(temperatures_oi$temperature),
             time = list(temperatures_oi$time)) %>%
      unnest(cols = c(temperatures, time))

    ## Calculate the intrinsic growth rate for each species
    ## at each temperature in the time series
    species_pars2 <- species_pars1 %>%
      mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperatures))

    ## calculate the change in growth rate from one time point to the next
    species_pars3 <- species_pars2 %>%
      group_by(case_id, species_id) %>%
      mutate(igr_lag = lag(igr),
             delta_igr = igr - igr_lag) %>%
      select(-igr_lag)







  ## Write to database
    if(i == 1) {
      dbWriteTable(conn_delta_igr, "delta_igr", species_pars3, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_delta_igr, "delta_igr", species_pars3, append = TRUE)
    }

  }

  dbDisconnect(conn_derivs)
  dbDisconnect(conn_temperatures)

}
