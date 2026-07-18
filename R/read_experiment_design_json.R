#' Legacy JSON experiment-design reader
#'
#' This internal helper reads the old JSON formatted experiment design.
#' Most scalar values in the JSON file are expressions that can be evaluated
#' with `eval()` to get the values of the experiment design. Structured values,
#' such as list-based interaction specifications, are returned as-is.
#'
#' @param experiment_folder The folder where the experiment information is located
#' @param experiment_design_filename The name of the file that contains the experiment design. This file should be in the experiment_folder and should be a JSON file. The JSON file should contain a list of expressions that can be evaluated to get the values of the experiment design.
#'
#' @return Returns a named list of expressions or structured values.
#' @keywords internal
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
    if (is.atomic(expt_def[[i]]) && length(expt_def[[i]]) == 1) {
      expt_def[[i]] <- parse(text = as.character(expt_def[[i]]))
    }
  }

  return(expt_def)

}
