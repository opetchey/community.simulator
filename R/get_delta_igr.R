#' Calculate the difference in growth rate from one time point to the next
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The name of the experiment design file
#' @param every_t The time interval at which to calculate the difference
#' @param overwrite Logical. If `TRUE`, overwrite an existing delta-IGR database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_delta_igr <- function(experiment_folder,
                          experiment_design_filename,
                          every_t = 1,
                          overwrite = FALSE,
                          verbose = TRUE) {

  ## open connections and read in data
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  output_path <- paste0(experiment_folder, "delta_igr.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "delta-IGR database")
  conn_delta_igr <- DBI::dbConnect(RSQLite::SQLite(), output_path)


  ## Expand expt to make a species in row dataset
  i = 1
  for(i in 1:length(expt$case_id)) {

    ## Housekeeping
    if (verbose) {
      message("Calculating delta IGR for case ", i, " of ", length(expt$case_id))
    }
    case_id_oi <- expt$case_id[i]
    env_series_oi <- expt$env_series_id[i]

    temperatures_oi <- temperatures |>
      dplyr::filter(env_series_id == env_series_oi) |>
      dplyr::collect() |>
      dplyr::filter((time %% every_t) == 0)

    ## Get the community parameters for this case
    comm_pars_i <- expt$community_object[i][[1]]
    species_pars <- tibble::tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                                   species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                                   b_opt_i = comm_pars_i$b_opt_i,
                                   a_b_i = comm_pars_i$a_b_i,
                                   s_i = comm_pars_i$s_i,
                                   a_d_i = comm_pars_i$a_d_i,
                                   z_i = comm_pars_i$z_i)

    ## Combine species parameters with temperatures
    species_pars1 <- species_pars |>
      dplyr::mutate(temperatures = list(temperatures_oi$temperature),
                    time = list(temperatures_oi$time)) |>
      tidyr::unnest(cols = c(temperatures, time))

    ## Calculate the intrinsic growth rate for each species
    ## at each temperature in the time series
    species_pars2 <- species_pars1 |>
      dplyr::mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                                    b_opt_i,
                                                    s_i,
                                                    a_d_i,
                                                    z_i,
                                                    temperatures))

    ## calculate the change in growth rate from one time point to the next
    species_pars3 <- species_pars2 |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::mutate(igr_lag = dplyr::lag(igr),
                    delta_igr = igr - igr_lag) |>
      dplyr::select(-igr_lag)







  ## Write to database
    if(i == 1) {
      DBI::dbWriteTable(conn_delta_igr, "delta_igr", species_pars3, overwrite = TRUE)
    }
    if(i > 1) {
      DBI::dbWriteTable(conn_delta_igr, "delta_igr", species_pars3, append = TRUE)
    }

  }

  DBI::dbDisconnect(conn_delta_igr)
  DBI::dbDisconnect(conn_temperatures)
  announce_output_written(output_path, verbose = verbose, label = "delta-IGR database")

}
