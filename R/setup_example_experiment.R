#' Set up an experiment folder from a bundled example
#'
#' This convenience helper creates an experiment folder and copies a bundled
#' example experiment-definition JSON file into it.
#'
#' @param experiment_folder_location Location where the experiment folder should
#'   be created.
#' @param experiment_name Name of the experiment folder.
#' @param example_experiment_name Name of the bundled example experiment to copy
#'   from. Defaults to `"test_experiment1"`.
#' @param experiment_design_filename Name of the bundled JSON design file to
#'   copy into the experiment folder.
#' @param verbose Logical. If `TRUE`, print setup messages.
#'
#' @return Invisibly returns a named list containing the experiment folder, the
#'   source design path, and the copied design path.
#' @export
#'
#' @examples NULL
setup_example_experiment <- function(experiment_folder_location,
                                     experiment_name,
                                     example_experiment_name = "test_experiment1",
                                     experiment_design_filename,
                                     verbose = TRUE) {

  experiment_folder <- create_experiment_folder(
    experiment_folder_location = experiment_folder_location,
    experiment_name = experiment_name,
    verbose = verbose
  )

  design_source <- system.file(
    "test_experiments",
    example_experiment_name,
    experiment_design_filename,
    package = "community.simulator"
  )

  if (design_source == "") {
    stop(
      paste0(
        "Bundled example design file not found: ",
        file.path("test_experiments", example_experiment_name, experiment_design_filename),
        "\nCheck the example experiment name and design filename."
      ),
      call. = FALSE
    )
  }

  design_target <- file.path(experiment_folder, experiment_design_filename)

  if (file.exists(design_target)) {
    stop(
      paste0(
        "Experiment design file already exists: ", design_target,
        "\nThe setup helper will not overwrite an existing design file."
      ),
      call. = FALSE
    )
  }

  copied <- file.copy(
    from = design_source,
    to = design_target,
    overwrite = FALSE
  )

  if (!copied) {
    stop(
      paste0("Failed to copy example design file to: ", design_target),
      call. = FALSE
    )
  }

  if (verbose) {
    message("Copied example design file to: ", design_target)
  }

  invisible(
    list(
      experiment_folder = experiment_folder,
      design_source = design_source,
      design_target = design_target
    )
  )
}
