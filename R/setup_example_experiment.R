#' Set up an experiment folder from a bundled example
#'
#' This convenience helper creates an experiment folder and copies a bundled
#' YAML experiment template into it.
#'
#' @param experiment_folder_location Location where the experiment folder should
#'   be created.
#' @param experiment_name Name of the experiment folder.
#' @param example_experiment_name Name of the bundled example experiment to copy
#'   from. Defaults to `"lv_discrete"`. Available templates include
#'   `"lv_discrete"`, `"lv_continuous"`, `"consumer_resource"`, and their
#'   `"_rich"` variants.
#' @param experiment_design_filename Name for the copied YAML specification.
#'   Defaults to `paste0(example_experiment_name, ".yaml")`.
#' @param verbose Logical. If `TRUE`, print setup messages.
#'
#' @return Invisibly returns a named list containing the experiment folder, the
#'   source design path, and the copied design path.
#' @export
#'
#' @examples NULL
setup_example_experiment <- function(experiment_folder_location,
                                     experiment_name,
                                     example_experiment_name = "lv_discrete",
                                     experiment_design_filename = NULL,
                                     verbose = TRUE) {

  experiment_folder <- create_experiment_folder(
    experiment_folder_location = experiment_folder_location,
    experiment_name = experiment_name,
    verbose = verbose
  )

  if (is.null(experiment_design_filename)) {
    experiment_design_filename <- paste0(example_experiment_name, ".yaml")
  }

  template_filename <- paste0(example_experiment_name, ".yaml")
  design_source <- system.file(
    "experiment_templates",
    template_filename,
    package = "community.simulator"
  )

  if (design_source == "") {
    stop(
      paste0(
        "Bundled example design file not found: ",
        file.path("experiment_templates", template_filename),
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
