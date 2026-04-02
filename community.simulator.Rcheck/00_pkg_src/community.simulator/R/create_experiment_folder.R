#' Create folder for experiment
#'
#' @param experiment_folder_location Location of experiment folder
#' @param experiment_name Name of experiment
#' @param verbose Logical. If `TRUE`, print messages about folder creation.
#'
#' @return A message indicating if the folder was created or if it already exists, and the location of the folder
#' @export
#'
#' @examples NULL
create_experiment_folder <- function(experiment_folder_location,
                                     experiment_name,
                                     verbose = TRUE) {

  ## make a folder for the experiment
  experiment_folder <- paste0(experiment_folder_location, "/", experiment_name, "/")

  if (dir.exists(experiment_folder) && verbose) {
    message("Experiment folder already exists: ", experiment_folder)
  }
  if(!dir.exists(experiment_folder)) {
    dir.create(experiment_folder)
    if (verbose) {
      message("Experiment folder created at: ", experiment_folder)
    }
  }

  return(experiment_folder)

}
