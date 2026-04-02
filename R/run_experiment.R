#' Run a complete experiment workflow
#'
#' This is a convenience wrapper for the standard experiment workflow. It
#' creates the experiment folder, builds the experiment table, generates
#' environmental time series, simulates dynamics, and calculates community-level
#' summary measures.
#'
#' @param experiment_folder_location Location where the experiment folder should
#'   be created.
#' @param experiment_name Name of the experiment folder.
#' @param experiment_design_filename Name of the experiment definition file.
#'   This file must already be present inside the experiment folder.
#' @param overwrite Logical. If `TRUE`, overwrite existing workflow outputs.
#' @param verbose Logical. If `TRUE`, print progress messages during the
#'   workflow.
#'
#' @return Invisibly returns a named list containing the experiment folder and
#'   the main output file paths.
#' @export
#'
#' @examples NULL
run_experiment <- function(experiment_folder_location,
                           experiment_name,
                           experiment_design_filename,
                           overwrite = FALSE,
                           verbose = TRUE) {

  if (verbose) {
    message("Creating or locating experiment folder")
  }
  experiment_folder <- create_experiment_folder(
    experiment_folder_location = experiment_folder_location,
    experiment_name = experiment_name,
    verbose = verbose
  )
  design_path <- file.path(experiment_folder, experiment_design_filename)

  if (!file.exists(design_path)) {
    stop(
      paste0(
        "Experiment design file not found: ", design_path,
        "\nCopy the JSON file into the experiment folder before calling run_experiment()."
      ),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Creating experiment table")
  }
  create_experiment_table(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message("Creating environments")
  }
  create_environments(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message("Simulating dynamics")
  }
  simulate_dynamics(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {
    message("Calculating community measures")
  }
  get_community_measures(
    experiment_folder,
    experiment_design_filename,
    overwrite = overwrite,
    verbose = verbose
  )

  outputs <- list(
    experiment_folder = experiment_folder,
    experiment_table = file.path(experiment_folder, "experiment_table.RDS"),
    temperatures_db = file.path(experiment_folder, "temperatures.db"),
    dynamics_db = file.path(experiment_folder, "dynamics.db"),
    community_measures = file.path(experiment_folder, "community_measures.RDS")
  )

  if (verbose) {
    message("Experiment workflow complete")
  }

  invisible(outputs)
}
