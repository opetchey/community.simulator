#' Create temperature times series. Currently three options for how times series vary among cases.
#' 1) `same_per_replicate` replicates of the same number share the same environmental time series. E.g., `case1_rep1` and `case2_rep1` share the same time series.
#' 2) `all_different` all environmental time series are different
#'
#' @param experiment_folder The folder where all information about the experiment is stored
#' @param experiment_design_filename The filename of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing temperatures database.
#' @param verbose Logical. If `TRUE`, print messages about written outputs.
#'
#' @return Returns nothing. Saves to SQLite databases the temperature time series.
#' @export
#'
#' @examples NULL
create_environments <- function(experiment_folder,
                                experiment_design_filename,
                                overwrite = FALSE,
                                verbose = TRUE) {

  require_dbplyr()

  ## setup the databases for saving the temperature time series
  output_path <- paste0(experiment_folder, "temperatures.db")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "temperatures database")
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(), output_path)

  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))

  environments <- expt |>
    dplyr::select(env_series_id, temperature_mean, temperature_sd, one_over_f_gamma, temperature_seed) |>
    dplyr::distinct()

  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  #expt_def$random_seed


  i <- 1

  for(i in 1:nrow(environments)) {

    set.seed(environments$temperature_seed[i])

    temperature_series <- tibble::tibble(phase = c(rep("burn_in", expt_def$burn_in_duration),
                                                   rep("expt", expt_def$experiment_duration + 1)),

                                       time = 0:(expt_def$burn_in_duration +
                                                   expt_def$experiment_duration),

                                       temperature = c(
                                         rep(expt_def$temperature_mean,
                                             expt_def$burn_in_duration),
                                         scale(
                                           primer::one_over_f(
                                             gamma = environments$one_over_f_gamma[i],
                                             N = expt_def$experiment_duration + 1
                                           )
                                         ) * expt_def$temperature_sd + expt_def$temperature_mean
                                       ),

                                       env_series_id = environments$env_series_id[i]) |>
      dplyr::mutate(temperature = ifelse(
        phase == "burn_in",
        temperature[expt_def$burn_in_duration + 1],
        temperature
      ))





    temperature_series_expt_only <- temperature_series |>
      dplyr::filter(time > expt_def$burn_in_duration)

    if(i == 1) {
      DBI::dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, overwrite = TRUE)
    }
    if(i > 1) {
      DBI::dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, append = TRUE)
    }



  }



  DBI::dbDisconnect(conn_temperatures)
  announce_output_written(output_path, verbose = verbose, label = "temperatures database")

}
