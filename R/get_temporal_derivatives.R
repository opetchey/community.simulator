#' Calculate the derivatives of the growth rate - temperature relationship at the temperatures in the temperature times series of each case in the experiment.
#'
#' @param experiment_folder The folder where the experiment is stored
#' @param experiment_design_filename The name of the experiment design file
#' @param every_t The time interval at which to calculate the derivatives
#' @param overwrite Logical. If `TRUE`, overwrite an existing temporal-derivatives database.
#' @param verbose Logical. If `TRUE`, print progress and output messages.
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_temporal_derivatives <- function(experiment_folder,
                                     experiment_design_filename,
                                     every_t = 10,
                                     overwrite = FALSE,
                                     verbose = TRUE) {

  ## open connections and read in data
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "temperatures.db"))
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## set up data base to save results into
  output_path <- paste0(experiment_folder, "temporal_derivs.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "temporal-derivatives database")
  conn_derivs <- DBI::dbConnect(RSQLite::SQLite(), output_path)


  ## Expand expt to make a species in row dataset
  i = 1
  for(i in 1:length(expt$case_id)) {

    ## Housekeeping
    if (verbose) {
      message("Calculating temporal derivatives for case ", i, " of ", length(expt$case_id))
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
      dplyr::mutate(temperatures = list(temperatures_oi$temperature)) |>
      tidyr::unnest(cols = temperatures)

    ## Calculate the intrinsic growth rate for each species
    ## at each temperature in the time series
    species_pars2 <- species_pars1 |>
      dplyr::mutate(igr = intrinsic_growth_gaussian(a_b_i,
                                                    b_opt_i,
                                                    s_i,
                                                    a_d_i,
                                                    z_i,
                                                    temperatures))

    ## Fit a gam to the growth rate - temperature relationship
    species_pars3 <- species_pars2 |>
      dplyr::nest_by(case_id, species_id) |>
      dplyr::mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 10),
                                            data = data))) |>
      dplyr::select(-data)

    ## make a ggplot of one of the species igr temperature relationships
    #species_pars2 |>
    #  filter(species_id == "Spp-2") |>
    #  ggplot(aes(x = temperatures, y = igr)) +
    #  geom_point()

    ## Calculate the derivative of the gam of the growth rate - temperature relationship
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


    ## make a ggplot of one of the species igr temperature relationships
    #species_pars4 |>
    #  filter(species_id == "Spp-1") |>
    #  ggplot(aes(x = temperature, y = igr)) +
    #  geom_line(col= "red") +
    #  geom_line(aes(y = derivative), col = "blue")


  ## Write to database
    if(i == 1) {
      DBI::dbWriteTable(conn_derivs, "derivs", species_pars4, overwrite = TRUE)
    }
    if(i > 1) {
      DBI::dbWriteTable(conn_derivs, "derivs", species_pars4, append = TRUE)
    }

  }

  DBI::dbDisconnect(conn_derivs)
  DBI::dbDisconnect(conn_temperatures)
  announce_output_written(output_path, verbose = verbose, label = "temporal-derivatives database")

}
