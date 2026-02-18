#' Get various community level measures, e.g., community stability, response diversity, position of optimal temperature, etc.
#'
#' @param experiment_folder The folder containing the experiment data
#' @param experiment_design_filename The name of the experiment design file
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_community_measures <- function(experiment_folder,
                                   experiment_design_filename) {


  ## Read in experiment information
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## open connections to databases
  ## temperatures
  conn_temperatures <- dbConnect(RSQLite::SQLite(),
                                 paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")
  ## dynamics
  conn_dynamics <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "dynamics.db"))
  dynamics <- tbl(conn_dynamics, "dynamics")
  ## temporal derivatives
  conn_temp_derivs <- dbConnect(RSQLite::SQLite(),
                                paste0(experiment_folder, "temporal_derivs.db"))
  temp_derivs <- tbl(conn_temp_derivs, "derivs")
  ## arbitrary derivatives
  conn_arb_derivs <- dbConnect(RSQLite::SQLite(),
                               paste0(experiment_folder, "arbitrary_derivs.db"))
  arb_derivs <- tbl(conn_arb_derivs, "derivs")

  # conn_delta_igr <- dbConnect(RSQLite::SQLite(),
  #                              paste0(experiment_folder, "delta_igr.db"))
  # delta_igr <- tbl(conn_delta_igr, "delta_igr")


  ### Calculate various community level measure

  ## Community total biomass CV
  comm_cv <- get_community_CV(dynamics)

  ## Community temperature sensitivity
  comm_temp_sens <- get_community_temp_sens(dynamics,
                                            temperatures,
                                            rollsumr_window = 50,
                                            expt)
  # comm_resp_div <- get_community_response_diversity(temp_derivs)

  ## Get community sum of relative b_opt
  comm_sum_rel_b_opt <- get_community_sum_rel_b_opt(temperatures, expt)
  # comm_sum_derivs <- get_community_sum_derivatives(arb_derivs, temp_derivs, delta_igr)

  # get community synchrony
  comm_syn<-get_community_syn(dynamics)

  # get pop_stab
  comm_pop<-get_community_popstab(dynamics)

  ## join all the community measures
  comm_measures <- expt |>
    full_join(comm_cv) |>
    full_join(comm_temp_sens) |>
    # full_join(comm_resp_div) |>
    full_join(comm_sum_rel_b_opt) |>
    # full_join(comm_sum_derivs)|>
    full_join(comm_syn) |>
    full_join(comm_pop)

  saveRDS(comm_measures, paste0(experiment_folder, "community_measures.RDS"))

}
