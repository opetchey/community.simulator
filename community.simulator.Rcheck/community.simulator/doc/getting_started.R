## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(community.simulator)

## -----------------------------------------------------------------------------
experiment_folder_location <- tempdir()
experiment_name <- "getting_started_example"
experiment_design_filename <- "getting_started_experiment.json"

experiment_folder <- create_experiment_folder(
  experiment_folder_location = experiment_folder_location,
  experiment_name = experiment_name,
  verbose = FALSE
)

design_source <- system.file(
  "extdata",
  experiment_design_filename,
  package = "community.simulator"
)

file.copy(
  from = design_source,
  to = file.path(experiment_folder, experiment_design_filename),
  overwrite = TRUE
)

outputs <- suppressWarnings(
  suppressMessages(
    run_experiment(
      experiment_folder_location = experiment_folder_location,
      experiment_name = experiment_name,
      experiment_design_filename = experiment_design_filename,
      overwrite = TRUE,
      verbose = FALSE
    )
  )
)

outputs

## -----------------------------------------------------------------------------
community_measures <- readRDS(outputs$community_measures)

community_measures[, c("case_id", "richness", "temperature_mean", "CV_totab")]

