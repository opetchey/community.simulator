#' Get derivatives at arbitrary temperatures
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The file with the experiment design
#' @param overwrite Logical. If `TRUE`, overwrite an existing arbitrary-derivatives database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @return Data base with the derivatives at arbitrary temperatures
#' @export
#'
#' @examples NULL
get_arbitrary_derivatives <- function(experiment_folder,
                                     experiment_design_filename,
                                     overwrite = FALSE,
                                     verbose = TRUE) {

  ## open connections and read in data
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  output_path <- paste0(experiment_folder, "arbitrary_derivs.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "arbitrary-derivatives database")
  conn_derivs <- DBI::dbConnect(RSQLite::SQLite(), output_path)


  ## For each case
  i <- 1
  for(i in 1:length(expt$case_id)) {

    if (verbose) {
      message("Calculating arbitrary derivatives for case ", i, " of ", length(expt$case_id))
    }

    ## Get a sequences of temperatures, using mean and standard deviation
    ## from the expt table
    min_temperature <- expt$temperature_mean[i] - 2.5 * expt$temperature_sd[i]
    max_temperature <- expt$temperature_mean[i] + 2.5 * expt$temperature_sd[i]

    temperatures_oi <- tibble::tibble(temperature = seq(min_temperature,
                                                        max_temperature,
                                                        length = 100))

    ## Get the parameters for each species in the community
    comm_pars_i <- expt$community_object[i][[1]]
    species_pars <- tibble::tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                                   species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                                   b_opt_i = comm_pars_i$b_opt_i,
                                   a_b_i = comm_pars_i$a_b_i,
                                   s_i = comm_pars_i$s_i,
                                   a_d_i = comm_pars_i$a_d_i,
                                   z_i = comm_pars_i$z_i)

    ## Combine the parameters with the temperatures
    species_pars1 <- species_pars |>
      dplyr::mutate(temperatures = list(temperatures_oi$temperature)) |>
      tidyr::unnest(cols = temperatures)

    ## Calculate the intrinsic growth rate for each species at each temperature
    species_pars2 <- species_pars1 |>
      dplyr::mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                                    b_opt_i,
                                                    s_i,
                                                    a_d_i,
                                                    z_i,
                                                    temperatures))

    ## Fit a GAM to the intrinsic growth rate - temperature relationship
    species_pars3 <- species_pars2 |>
      dplyr::nest_by(case_id, species_id) |>
      dplyr::mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 10),
                                            data = data))) |>
      dplyr::select(-data)

    ## Get the derivatives of the GAM at the arbitrary temperatures
    species_pars4 <- dplyr::full_join(species_pars1, species_pars3) |>
      dplyr::group_by(case_id, species_id) |>
      dplyr::mutate(new_data = list(data.frame(temperatures = temperatures))) |>
      dplyr::select(-temperatures) |>
      unique() |>
      dplyr::rowwise() |>
      dplyr::mutate(derivative = list(gratia::derivatives(models,
                                                          data = new_data))) |>
      tidyr::unnest(derivative) |>
      dplyr::select(-models, -new_data) |>
      dplyr::rename(temperature = temperatures, derivative = .derivative) |>
      dplyr::mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                                    b_opt_i,
                                                    s_i,
                                                    a_d_i,
                                                    z_i,
                                                    temperature)) |>
      dplyr::select(case_id, species_id, temperature, igr, derivative)



    if(i == 1) {
      DBI::dbWriteTable(conn_derivs, "derivs", species_pars4, overwrite = TRUE)
    }
    if(i > 1) {
      DBI::dbWriteTable(conn_derivs, "derivs", species_pars4, append = TRUE)
    }

  }

  DBI::dbDisconnect(conn_derivs)
  DBI::dbDisconnect(conn_temperatures)
  announce_output_written(output_path, verbose = verbose, label = "arbitrary-derivatives database")



}
