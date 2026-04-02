#' Read in the JSON formatted text file that contains the experiment design. All values in the JSON file must be expressions that can be evaluated (using the `eval` function) to get the values of the experiment design.
#'
#' @param experiment_folder The folder where the experiment information is located
#' @param experiment_design_filename The name of the file that contains the experiment design. This file should be in the experiment_folder and should be a JSON file. The JSON file should contain a list of expressions that can be evaluated to get the values of the experiment design.
#'
#' @return Returns a named list of expressions that can be evaluated to get the values of the experiment design
#' @export
#'
#' @examples NULL
read_experiment_design_json <- function(experiment_folder, experiment_design_filename){

  experiment_folder <- path.expand(experiment_folder)
  design_path <- file.path(experiment_folder, experiment_design_filename)

  if (!file.exists(design_path)) {
    stop(
      paste0(
        "Experiment design file not found: ", design_path,
        "\nCheck that the file exists inside the experiment folder."
      ),
      call. = FALSE
    )
  }

  expt_def <- jsonlite::fromJSON(design_path)

  for(i in 1:length(expt_def)) {
    expt_def[[i]] <- parse(text = expt_def[[i]])
  }

  return(expt_def)

}
