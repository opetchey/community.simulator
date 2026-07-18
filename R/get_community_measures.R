#' Legacy JSON community-measures calculator
#'
#' This internal helper calculates community measures for the old JSON
#' experiment format. New user-facing workflows should use
#' [get_community_measures_from_spec()] through [run_experiment()].
#'
#' @param experiment_folder The folder containing the experiment data
#' @param experiment_design_filename The name of the experiment design file
#' @param overwrite Logical. If `TRUE`, overwrite an existing community-measures file.
#' @param verbose Logical. If `TRUE`, print messages about written outputs.
#'
#' @details If `simulation_summaries.RDS` is present, dynamic abundance
#'   summaries are read from that compact file. Otherwise, the function falls
#'   back to calculating those summaries from `dynamics.db`. Old experiment
#'   JSON files can include `parallel_community_measures` and `parallel_workers`
#'   to calculate community performance curve measures in parallel where
#'   supported.
#'
#' @return Nothing. Saves data to a file.
#' @keywords internal
#'
#' @examples NULL
get_community_measures <- function(experiment_folder,
                                   experiment_design_filename,
                                   overwrite = FALSE,
                                   verbose = TRUE) {

  require_dbplyr()

  ## Read in experiment information
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))
  output_path <- paste0(experiment_folder, "community_measures.RDS")
  prepare_output_path(output_path, overwrite = overwrite, verbose = verbose, label = "community-measures file")

  get_design_value <- function(name, default) {
    value <- expt_def[[name]]
    if (is.null(value) || length(value) == 0) {
      return(default)
    }
    value <- value[[1]]
    if (is.character(value)) {
      parsed_value <- try(eval(parse(text = value)), silent = TRUE)
      if (!inherits(parsed_value, "try-error")) {
        return(parsed_value)
      }
    }
    value
  }

  parallel_community_measures <- isTRUE(get_design_value(
    "parallel_community_measures",
    get_design_value("parallel_simulations", FALSE)
  ))
  parallel_workers <- as.integer(get_design_value(
    "parallel_workers",
    max(1, parallel::detectCores(logical = FALSE) - 1)
  ))
  if (is.na(parallel_workers)) {
    parallel_workers <- 1
  }
  parallel_workers <- max(1, min(parallel_workers, nrow(expt)))

  ## open connections to databases
  ## temperatures
  conn_temperatures <- DBI::dbConnect(RSQLite::SQLite(),
                                      paste0(experiment_folder, "temperatures.db"))
  on.exit(DBI::dbDisconnect(conn_temperatures), add = TRUE)
  temperatures <- dplyr::tbl(conn_temperatures, "temperatures")
  summaries_path <- paste0(experiment_folder, "simulation_summaries.RDS")
  dynamics_path <- paste0(experiment_folder, "dynamics.db")
  has_simulation_summaries <- file.exists(summaries_path)
  has_dynamics <- file.exists(dynamics_path)

  if (!has_simulation_summaries && !has_dynamics) {
    stop(
      "Cannot calculate community measures because neither simulation_summaries.RDS ",
      "nor dynamics.db was found.",
      call. = FALSE
    )
  }

  dynamics <- NULL
  if (!has_simulation_summaries) {
    conn_dynamics <- DBI::dbConnect(RSQLite::SQLite(), dynamics_path)
    on.exit(DBI::dbDisconnect(conn_dynamics), add = TRUE)
    dynamics <- dplyr::tbl(conn_dynamics, "dynamics")
  }
  ## temporal derivatives
  #conn_temp_derivs <- dbConnect(RSQLite::SQLite(),
  #                              paste0(experiment_folder, "temporal_derivs.db"))
  #temp_derivs <- tbl(conn_temp_derivs, "derivs")
  ## arbitrary derivatives
  #conn_arb_derivs <- dbConnect(RSQLite::SQLite(),
  #                             paste0(experiment_folder, "arbitrary_derivs.db"))
  #arb_derivs <- tbl(conn_arb_derivs, "derivs")

  # conn_delta_igr <- dbConnect(RSQLite::SQLite(),
  #                              paste0(experiment_folder, "delta_igr.db"))
  # delta_igr <- tbl(conn_delta_igr, "delta_igr")


  ### Calculate various community level measure

  if (has_simulation_summaries) {
    comm_dynamic_measures <- readRDS(summaries_path) |>
      tibble::as_tibble()
  } else {
    ## Community total biomass CV
    comm_cv <- get_community_CV(dynamics)

    ## Community temperature sensitivity
    comm_temp_sens <- get_community_temp_sens(dynamics,
                                              temperatures,
                                              rollsumr_window = 50,
                                              expt)
    # comm_resp_div <- get_community_response_diversity(temp_derivs)

    # get community synchrony
    comm_syn <- get_community_syn(dynamics)

    # get pop_stab
    comm_pop <- get_community_popstab(dynamics)

    comm_dynamic_measures <- comm_cv |>
      dplyr::left_join(comm_temp_sens) |>
      dplyr::left_join(comm_syn) |>
      dplyr::left_join(comm_pop)
  }

  ## Get community sum of relative b_opt
  comm_sum_rel_b_opt <- get_community_sum_rel_b_opt(temperatures, expt)
  # comm_sum_derivs <- get_community_sum_derivatives(arb_derivs, temp_derivs, delta_igr)

  # get community CPC measures
  comm_cpc <- get_community_CPC_measures(temperatures,
                                   expt,
                                   expt_def,
                                   every_t = 1,
                                   parallel_community_measures = parallel_community_measures,
                                   parallel_workers = parallel_workers,
                                   verbose = verbose)



  ## join all the community measures
  comm_measures <- expt |>
    dplyr::left_join(comm_dynamic_measures) |>
    # full_join(comm_resp_div) |>
    dplyr::left_join(comm_sum_rel_b_opt) |>
    # full_join(comm_sum_derivs)|>
    dplyr::left_join(comm_cpc)

  saveRDS(comm_measures, output_path)
  announce_output_written(output_path, verbose = verbose, label = "community-measures file")

}
